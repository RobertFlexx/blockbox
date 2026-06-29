package blockbox.net;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Locale;

public final class BlockboxNet {
  public static final int PROTOCOL_VERSION = 2;
  public static final int MAX_LINE_LENGTH = 8192;
  public static final int MAX_PACKET_PARTS = 64;
  public static final int MAX_FIELD_LENGTH = MAX_LINE_LENGTH;
  public static final int MAX_NAME_LENGTH = 16;
  public static final int MAX_FLOAT_TEXT_LENGTH = 64;
  public static final int CONNECT_TIMEOUT_MS = 10_000;
  public static final int JOIN_TIMEOUT_MS = 120_000;
  public static final int MAX_PLAYERS = 32;
  public static final int BAD_PACKET_LIMIT = 20;
  public static final int CLIENT_QUEUE_LIMIT = 300_000;
  public static final String VANILLA_MOD_HASH = "vanilla";
  public static final String PING = "PING";
  public static final String HELO = "HELO";
  public static final String ERR = "ERR";
  public static final String WORLD = "WORLD";
  public static final String PLAYERS = "PLAYERS";
  public static final String OPLIST = "OPLIST";
  public static final String SNAPBEGIN = "SNAPBEGIN";
  public static final String SNAPEND = "SNAPEND";
  public static final String ENTER = "ENTER";
  public static final String BLOC = "BLOC";
  public static final String POS = "POS";

  private BlockboxNet() {}

  public static final class Packet {
    private final String raw;
    private final String[] parts;

    private Packet(String raw) {
      this.raw = raw == null ? "" : raw;
      if (!isSafeProtocolLine(this.raw)) throw new IllegalArgumentException("invalid protocol line");
      this.parts = split(this.raw);
      if (this.parts.length > MAX_PACKET_PARTS) throw new IllegalArgumentException("too many packet fields");
      if (!isPacketType(type())) throw new IllegalArgumentException("invalid packet type: " + type());
    }

    public String raw() { return raw; }
    public String type() { return parts.length == 0 ? "" : parts[0]; }
    public String[] parts() { return Arrays.copyOf(parts, parts.length); }
    public int size() { return parts.length; }
    public String part(int index) { return parts[index]; }
    public boolean is(String packetType) { return packetType != null && packetType.equals(type()); }
    public void require(int count) { requireParts(parts, count, type()); }
    public void requireExact(int count) { requireExactParts(parts, count, type()); }
  }

  public static final class HelloPacket {
    public final boolean modern;
    public final boolean badProtocol;
    public final String playerName;
    public final String modHash;
    public final int clientProtocol;

    private HelloPacket(boolean modern, boolean badProtocol, String playerName, String modHash, int clientProtocol) {
      this.modern = modern;
      this.badProtocol = badProtocol;
      this.playerName = playerName;
      this.modHash = modHash;
      this.clientProtocol = clientProtocol;
    }
  }

  public static final class BlockPacket {
    public final int x;
    public final int y;
    public final int z;
    public final int blockId;
    public final int waterLevel;

    private BlockPacket(int x, int y, int z, int blockId, int waterLevel) {
      this.x = x;
      this.y = y;
      this.z = z;
      this.blockId = blockId;
      this.waterLevel = waterLevel;
    }
  }

  public static final class PositionPacket {
    public final String name;
    public final float x;
    public final float y;
    public final float z;
    public final float yaw;
    public final float pitch;
    public final int color;

    private PositionPacket(String name, float x, float y, float z, float yaw, float pitch, int color) {
      this.name = name;
      this.x = x;
      this.y = y;
      this.z = z;
      this.yaw = yaw;
      this.pitch = pitch;
      this.color = color;
    }
  }

  public static final class SnapshotVerifier {
    private int expected = -1;
    private int seen = 0;
    private long checksum = 0L;
    private boolean complete = false;
    private boolean valid = true;

    public void begin(int expected) {
      if (expected < 0) throw new IllegalArgumentException("negative snapshot size");
      this.expected = expected;
      this.seen = 0;
      this.checksum = 0L;
      this.complete = false;
      this.valid = true;
    }

    public void add(BlockPacket block) {
      if (expected < 0 || complete) return;
      checksum = blockChecksum(checksum, block.x, block.y, block.z, block.blockId, block.waterLevel);
      seen++;
    }

    public void end(Packet packet) {
      if (expected < 0) throw new IllegalStateException("SNAPEND before SNAPBEGIN");
      if (packet.size() >= 3) {
        int serverSeen = parseInt(packet.part(1), 0, Integer.MAX_VALUE);
        String serverChecksum = packet.part(2);
        valid = expected == seen && serverSeen == seen && serverChecksum.equalsIgnoreCase(BlockboxNet.checksumText(checksum));
      } else {
        valid = expected == seen;
      }
      complete = true;
    }

    public boolean complete() { return complete; }
    public boolean valid() { return complete && valid; }
    public int expected() { return expected; }
    public int seen() { return seen; }
    public String checksumText() { return BlockboxNet.checksumText(checksum); }
  }

  public static Packet packet(String line) {
    return new Packet(line);
  }

  public static boolean isPacketLine(String line) {
    if (line == null || line.length() > MAX_LINE_LENGTH || !isSafeProtocolLine(line)) return false;
    try {
      packet(line);
      return true;
    } catch (IllegalArgumentException ignored) {
      return false;
    }
  }

  public static String readProtocolLine(BufferedReader in) throws IOException {
    if (in == null) throw new IOException("missing protocol reader");
    StringBuilder line = new StringBuilder(Math.min(256, MAX_LINE_LENGTH));
    while (true) {
      int ch = in.read();
      if (ch < 0) {
        if (line.length() == 0) return null;
        break;
      }
      if (ch == '\n') break;
      if (ch == '\r') continue;
      if (line.length() >= MAX_LINE_LENGTH) throw new IOException("Protocol line too long");
      line.append((char) ch);
    }
    String result = line.toString();
    if (!isSafeProtocolLine(result)) {
      throw new IOException("Protocol line contains invalid control characters");
    }
    return result;
  }

  public static String[] split(String line) {
    String value = line == null ? "" : line;
    String[] out = new String[Math.min(MAX_PACKET_PARTS + 1, Math.max(1, value.length() + 1))];
    int count = 0;
    int start = 0;
    for (int i = 0; i <= value.length(); i++) {
      if (i == value.length() || value.charAt(i) == '|') {
        if (count == out.length) out = Arrays.copyOf(out, out.length + 8);
        out[count++] = value.substring(start, i);
        start = i + 1;
      }
    }
    return Arrays.copyOf(out, count);
  }

  public static boolean hasPrefix(String line, String prefix) {
    return line != null && (line.equals(prefix) || line.startsWith(prefix + "|"));
  }

  public static void requireParts(String[] parts, int count, String packetName) {
    if (parts == null || parts.length < count) {
      throw new IllegalArgumentException(packetName + " needs at least " + count + " fields");
    }
  }

  public static void requireExactParts(String[] parts, int count, String packetName) {
    if (parts == null || parts.length != count) {
      throw new IllegalArgumentException(packetName + " needs exactly " + count + " fields");
    }
  }

  public static String safeName(String value) {
    String input = value == null ? "" : unescape(value).trim();
    StringBuilder out = new StringBuilder(MAX_NAME_LENGTH);
    for (int i = 0; i < input.length() && out.length() < MAX_NAME_LENGTH; i++) {
      char ch = input.charAt(i);
      if (Character.isLetterOrDigit(ch) || ch == '_' || ch == '-') {
        out.append(ch);
      }
    }
    return out.length() == 0 ? "Player" : out.toString();
  }

  public static String escape(String value) {
    String input = value == null ? "" : value;
    StringBuilder out = new StringBuilder(input.length());
    for (int i = 0; i < input.length(); i++) {
      char ch = input.charAt(i);
      if (ch == '%') out.append("%25");
      else if (ch == '|') out.append("%7C");
      else if (ch == '\n' || ch == '\r' || ch == '\t' || ch == 0) out.append(' ');
      else out.append(ch);
    }
    return out.toString();
  }

  public static String unescape(String value) {
    String input = value == null ? "" : value;
    StringBuilder out = new StringBuilder(input.length());
    for (int i = 0; i < input.length(); i++) {
      char ch = input.charAt(i);
      if (ch == '%' && i + 2 < input.length()) {
        String code = input.substring(i + 1, i + 3).toUpperCase(Locale.ROOT);
        if ("25".equals(code)) {
          out.append('%');
          i += 2;
        } else if ("7C".equals(code)) {
          out.append('|');
          i += 2;
        } else {
          out.append(ch);
        }
      } else {
        out.append(ch);
      }
    }
    return out.toString();
  }

  public static String floatText(float value) {
    if (!Float.isFinite(value)) return "0.0";
    return Float.toString(value);
  }

  public static float parseFloat(String value) {
    if (value == null) throw new NumberFormatException("null");
    String clean = value.trim();
    if (clean.isEmpty()) throw new NumberFormatException("empty float");
    if (clean.length() > MAX_FLOAT_TEXT_LENGTH) throw new NumberFormatException("float text too long");
    float parsed = Float.parseFloat(clean.replace(',', '.'));
    if (!Float.isFinite(parsed)) throw new NumberFormatException("non-finite float: " + value);
    return parsed;
  }

  public static int parseInt(String value, int min, int max) {
    if (min > max) throw new IllegalArgumentException("invalid integer range");
    if (value == null) throw new NumberFormatException("null");
    String clean = value.trim();
    if (clean.isEmpty() || clean.length() > 16) throw new NumberFormatException("invalid integer text");
    int parsed = Integer.parseInt(clean);
    if (parsed < min || parsed > max) {
      throw new NumberFormatException(String.format(Locale.ROOT, "out of range: %d", parsed));
    }
    return parsed;
  }

  public static long parseLong(String value) {
    if (value == null) throw new NumberFormatException("null");
    String clean = value.trim();
    if (clean.isEmpty() || clean.length() > 32) throw new NumberFormatException("invalid long text");
    return Long.parseLong(clean);
  }

  public static HelloPacket parseHello(Packet packet) {
    requireType(packet, HELO);
    packet.require(2);
    boolean modern = packet.size() >= 4 && isInteger(packet.part(1));
    int clientProtocol = modern ? parseInt(packet.part(1), 0, Integer.MAX_VALUE) : 1;
    boolean badProtocol = modern && clientProtocol != PROTOCOL_VERSION;
    String playerName = modern ? safeName(packet.part(2)) : safeName(packet.part(1));
    String modHash = modern && packet.size() >= 4 ? unescape(packet.part(3)) : packet.size() >= 3 ? unescape(packet.part(2)) : VANILLA_MOD_HASH;
    if (modHash == null || modHash.isBlank()) modHash = VANILLA_MOD_HASH;
    return new HelloPacket(modern, badProtocol, playerName, modHash, clientProtocol);
  }

  public static BlockPacket parseBlock(Packet packet, int worldHeight) {
    requireType(packet, BLOC);
    if (worldHeight <= 0) throw new IllegalArgumentException("worldHeight must be positive");
    packet.require(5);
    int x = parseInt(packet.part(1), Integer.MIN_VALUE, Integer.MAX_VALUE);
    int y = parseInt(packet.part(2), 0, worldHeight - 1);
    int z = parseInt(packet.part(3), Integer.MIN_VALUE, Integer.MAX_VALUE);
    int blockId = parseInt(packet.part(4), Byte.MIN_VALUE, 255);
    int waterLevel = packet.size() >= 6 ? parseInt(packet.part(5), 1, 8) : 0;
    return new BlockPacket(x, y, z, blockId, waterLevel);
  }

  public static PositionPacket parsePosition(Packet packet, int colorCount) {
    requireType(packet, POS);
    packet.require(7);
    String name = safeName(packet.part(1));
    float x = parseFloat(packet.part(2));
    float y = parseFloat(packet.part(3));
    float z = parseFloat(packet.part(4));
    float yaw = parseFloat(packet.part(5));
    float pitch = parseFloat(packet.part(6));
    int colors = Math.max(1, colorCount);
    int color = packet.size() >= 8 ? Math.floorMod(parseInt(packet.part(7), Integer.MIN_VALUE, Integer.MAX_VALUE), colors) : 0;
    return new PositionPacket(name, x, y, z, yaw, pitch, color);
  }

  public static String playerToken(String name, int color, int colorCount) {
    return safeName(name) + ":" + Math.floorMod(color, Math.max(1, colorCount));
  }

  public static String pos(String name, float x, float y, float z, float yaw, float pitch, int color, int colorCount) {
    return line(POS, safeName(name), floatText(x), floatText(y), floatText(z), floatText(yaw), floatText(pitch), Integer.toString(Math.floorMod(color, Math.max(1, colorCount))));
  }

  public static String block(int x, int y, int z, int blockId) {
    return line(BLOC, Integer.toString(x), Integer.toString(y), Integer.toString(z), Integer.toString(blockId));
  }

  public static String block(int x, int y, int z, int blockId, int waterLevel) {
    return waterLevel > 0 ? line(BLOC, Integer.toString(x), Integer.toString(y), Integer.toString(z), Integer.toString(blockId), Integer.toString(clamp(waterLevel, 1, 8))) : block(x, y, z, blockId);
  }

  public static long blockChecksum(long current, int x, int y, int z, int blockId, int waterLevel) {
    long h = current == 0L ? 0xcbf29ce484222325L : current;
    h = mix(h, x);
    h = mix(h, y);
    h = mix(h, z);
    h = mix(h, blockId);
    h = mix(h, waterLevel);
    return h;
  }

  public static String checksumText(long checksum) {
    return Long.toUnsignedString(checksum, 16);
  }

  public static String world(long seed, float x, float y, float z, int color, String hostName, int hostColor, String sessionId) {
    return line(WORLD, Long.toString(seed), floatText(x), floatText(y), floatText(z), Integer.toString(color), safeName(hostName), Integer.toString(hostColor), escape(sessionId), Integer.toString(PROTOCOL_VERSION));
  }

  public static String helo(String playerName, String modHash) {
    String hash = modHash == null || modHash.isBlank() ? VANILLA_MOD_HASH : modHash;
    return line(HELO, Integer.toString(PROTOCOL_VERSION), safeName(playerName), escape(hash));
  }

  public static String error(String code, String message) {
    return line(ERR, escape(code == null ? "ERROR" : code), escape(message == null ? "" : message));
  }

  public static String snapBegin(int count) {
    return line(SNAPBEGIN, Integer.toString(Math.max(0, count)));
  }

  public static String snapEnd(int count, long checksum) {
    return line(SNAPEND, Integer.toString(Math.max(0, count)), checksumText(checksum));
  }

  public static String enter(String sessionId) {
    return line(ENTER, escape(sessionId));
  }

  public static String line(String type, String... fields) {
    if (!isPacketType(type)) throw new IllegalArgumentException("invalid packet type: " + type);
    StringBuilder out = new StringBuilder(type);
    if (fields != null) {
      if (fields.length + 1 > MAX_PACKET_PARTS) throw new IllegalArgumentException("too many packet fields");
      for (String field : fields) {
        String value = field == null ? "" : field;
        if (value.length() > MAX_FIELD_LENGTH) throw new IllegalArgumentException("packet field too long");
        if (!isSafeField(value)) throw new IllegalArgumentException("invalid packet field");
        out.append('|').append(value);
      }
    }
    String result = out.toString();
    if (result.length() > MAX_LINE_LENGTH) throw new IllegalArgumentException("packet too long");
    return result;
  }

  private static void requireType(Packet packet, String type) {
    if (packet == null || !packet.is(type)) throw new IllegalArgumentException("expected " + type);
  }

  private static boolean isSafeProtocolLine(String line) {
    if (line == null) return false;
    if (line.length() > MAX_LINE_LENGTH) return false;
    for (int i = 0; i < line.length(); i++) {
      char ch = line.charAt(i);
      if (ch == 0 || (Character.isISOControl(ch) && ch != '\t')) return false;
    }
    return true;
  }

  private static boolean isSafeField(String value) {
    return value.indexOf('|') < 0 && isSafeProtocolLine(value);
  }

  private static boolean isPacketType(String value) {
    if (value == null || value.isBlank() || value.length() > 24) return false;
    for (int i = 0; i < value.length(); i++) {
      char ch = value.charAt(i);
      if (!(ch >= 'A' && ch <= 'Z') && !(ch >= '0' && ch <= '9') && ch != '_') return false;
    }
    return true;
  }

  private static boolean isInteger(String value) {
    if (value == null || value.isEmpty()) return false;
    int start = value.charAt(0) == '-' ? 1 : 0;
    if (start == value.length()) return false;
    for (int i = start; i < value.length(); i++) {
      if (!Character.isDigit(value.charAt(i))) return false;
    }
    return true;
  }

  private static long mix(long hash, int value) {
    hash ^= value & 0xffffffffL;
    hash *= 0x100000001b3L;
    return hash;
  }

  private static int clamp(int value, int min, int max) {
    return Math.max(min, Math.min(max, value));
  }
}
