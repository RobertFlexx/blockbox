package blockbox;
// this is stupid helper me made, feel free to use and test
import blockbox.io.BlockboxFiles;
import blockbox.io.BlockboxSaveFiles;
import blockbox.net.BlockboxNet;
import blockbox.net.BlockboxSockets;
import blockbox.net.PacketBudget;
import blockbox.render.ViewFrustum;
import blockbox.render.ShaderSources;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.net.ServerSocket;
import java.nio.file.Files;
import java.nio.file.Path;

public final class BlockboxJavaHelpersTest {
  private BlockboxJavaHelpersTest() {}

  public static void main(String[] args) throws Exception {
    testFiles();
    testSaveFiles();
    testNet();
    testSockets();
    testFrustum();
    testPacketBudget();
    testShaderSources();
    System.out.println("BlockboxJavaHelpersTest: OK");
  }

  private static void testFiles() throws Exception {
    Path root = Files.createTempDirectory("blockbox-files-test");
    try {
      Path child = BlockboxFiles.safeChild(root, "worlds/save-1/index.txt");
      check(child.startsWith(root.toAbsolutePath().normalize()), "safe child stays under root");
      expectIOException(() -> BlockboxFiles.safeChild(root, "../escape.txt"), "safeChild rejects parent escape");
      expectIOException(() -> BlockboxFiles.safeChild(root, "/tmp/escape.txt"), "safeChild rejects absolute path");

      Path text = child;
      BlockboxFiles.writeTextAtomic(text, "hello");
      check("hello".equals(BlockboxFiles.readText(text, 16)), "atomic text round trip");
      expectIOException(() -> BlockboxFiles.readText(text, 2), "readText enforces byte limit");

      Path src = root.resolve("src");
      Path nested = src.resolve("a/b.txt");
      BlockboxFiles.writeTextAtomic(nested, "copy me");
      Path dst = root.resolve("dst");
      BlockboxFiles.copyRecursive(src, dst);
      check("copy me".equals(BlockboxFiles.readText(dst.resolve("a/b.txt"), 32)), "recursive copy preserves file contents");
      expectIOException(() -> BlockboxFiles.copyRecursive(src, src.resolve("inside")), "copyRecursive rejects target inside source");

      String hash1 = BlockboxFiles.sha256Tree(src);
      String hash2 = BlockboxFiles.sha256Tree(src);
      check(hash1.equals(hash2) && hash1.length() == 64, "tree hash is stable sha256 hex");
    } finally {
      BlockboxFiles.deleteRecursive(root);
    }
  }

  private static void testNet() throws Exception {
    check(BlockboxNet.parseHello(BlockboxNet.packet("HELO|3|Legacy|vanilla")).badProtocol, "old world generation protocol rejected");
    check(!BlockboxNet.parseHello(BlockboxNet.packet(BlockboxNet.helo("Player","vanilla"))).badProtocol, "current handshake accepted");
    String escaped = BlockboxNet.escape("a|b%c\n");
    check("a|b%c ".equals(BlockboxNet.unescape(escaped)), "escape/unescape round trip for protocol separators");

    String pos = BlockboxNet.pos("Bad Name!", 1.25f, 2f, 3f, 90f, -10f, 99, 8);
    BlockboxNet.Packet packet = BlockboxNet.packet(pos);
    BlockboxNet.PositionPacket parsed = BlockboxNet.parsePosition(packet, 8);
    check("BadName".equals(parsed.name), "position parser sanitizes player name");
    check(parsed.color == 3, "position parser normalizes color");

    BlockboxNet.BlockPacket block = BlockboxNet.parseBlock(BlockboxNet.packet(BlockboxNet.block(1, 2, 3, 4, 8)), 64);
    check(block.x == 1 && block.y == 2 && block.z == 3 && block.blockId == 4 && block.waterLevel == 8, "block parser accepts water level");
    BlockboxNet.BlockPacket torch = BlockboxNet.parseBlock(BlockboxNet.packet(BlockboxNet.block(1, 2, 3, 34, 0, 0, 4)), 64);
    check(torch.torchMount == 4, "block parser preserves torch attachment");
    expectIllegalArgument(() -> BlockboxNet.parseBlock(BlockboxNet.packet("BLOC|1|2|3|34|0|0|5"), 64), "invalid torch attachment rejected");
    expectIllegalArgument(() -> BlockboxNet.parseBlock(BlockboxNet.packet("BLOC|30000001|2|3|1"), 64), "block coordinates outside world bounds rejected");
    expectIllegalArgument(() -> BlockboxNet.parseBlock(BlockboxNet.packet("BLOC|1|2|3|-1"), 64), "negative block ID rejected");
    check(BlockboxNet.blockChecksum(0,1,2,3,34,0,0,1) != BlockboxNet.blockChecksum(0,1,2,3,34,0,0,2), "snapshot checksum includes torch attachment");
    expectIllegalArgument(() -> BlockboxNet.parseBlock(BlockboxNet.packet(BlockboxNet.block(1, 64, 3, 4)), 64), "block parser bounds y");
    expectIllegalArgument(() -> BlockboxNet.packet("bad|packet"), "packet rejects bad type");

    String line = BlockboxNet.readProtocolLine(new BufferedReader(new StringReader("PING\r\nNEXT\n")));
    check("PING".equals(line), "bounded protocol reader handles CRLF");
    expectIOException(() -> BlockboxNet.readProtocolLine(new BufferedReader(new StringReader("A".repeat(BlockboxNet.MAX_LINE_LENGTH + 1) + "\n"))), "bounded protocol reader rejects long line");
    expectIOException(() -> BlockboxNet.readProtocolLine(new BufferedReader(new StringReader("\r".repeat(BlockboxNet.MAX_LINE_LENGTH+2)+"\n"))), "carriage returns cannot bypass line budget");
    expectIllegalArgument(() -> BlockboxNet.parsePosition(BlockboxNet.packet("POS|Player|1e30|60|0|0|0|0"),10), "position rejects coordinates outside world bounds");
    var environment = BlockboxNet.parseEnvironment(BlockboxNet.packet("ENV|150|-1|0.5|0.7|0.8|-0.2|1"));
    check(environment.time() == 150 && environment.rain() == 0.5f && environment.timeOverride() == -1, "environment packet round trip");
    expectIllegalArgument(() -> BlockboxNet.parseEnvironment(BlockboxNet.packet("ENV|NaN|-1|0|0|0|0|0")), "environment rejects NaN");
    expectIllegalArgument(() -> BlockboxNet.parseEnvironment(BlockboxNet.packet("ENV|150|-1|2|0|0|0|0")), "environment rejects out of range rain");
    expectIllegalArgument(() -> BlockboxNet.parseEnvironment(BlockboxNet.packet("ENV|150|-1|0|0|0|0")), "environment rejects truncated state");
  }

  private static void testPacketBudget() {
    PacketBudget budget = new PacketBudget(10, 2, 1_000_000_000L);
    check(budget.allow(1_000_000_000L) && budget.allow(1_000_000_000L), "packet burst allowed");
    check(!budget.allow(1_000_000_000L), "packet burst bounded");
    check(!budget.allow(0), "backwards clock cannot refill budget");
    check(budget.allow(1_100_000_000L), "packet budget recovers over time");
    check(!budget.allow(1_100_000_000L), "refill does not create excess tokens");
  }

  private static void testFrustum() {
    ViewFrustum frustum = new ViewFrustum();
    float[] identity = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
    frustum.update(identity, identity);
    check(frustum.intersects(-1,-1,-1,1,1,1), "frustum includes its boundary");
    check(!frustum.intersects(2,2,2,3,3,3), "frustum rejects outside bounds");
    for (float aspect : new float[]{1, 16f/9, 32f/9}) {
      float f = (float)(1 / Math.tan(Math.toRadians(70) / 2));
      float near = 0.25f, far = 1200;
      float[] projection = {f/aspect,0,0,0, 0,f,0,0, 0,0,(far+near)/(near-far),-1, 0,0,2*far*near/(near-far),0};
      for (int yaw = 0; yaw < 360; yaw += 45) {
        double angle = Math.toRadians(yaw);
        float c = (float)Math.cos(angle), s = (float)Math.sin(angle);
        // R_x(90) R_y(yaw) T_y(-300), matching the game's camera matrix.
        float[] view = {c,s,0,0, 0,0,1,0, s,-c,0,0, 0,0,-300,1};
        frustum.update(projection,view);
        for (int x = -64; x <= 64; x += 16) {
          for (int z = -64; z <= 64; z += 16) {
            check(frustum.intersects(x,0,z,x+16,128,z+16), "aerial view retains ground in every yaw/aspect");
          }
        }
      }
      frustum.update(projection, identity);
      check(frustum.intersects(-1,-1,-20,1,1,-18), "forward terrain visible");
      check(!frustum.intersects(-1,-1,18,1,1,20), "terrain behind horizontal camera culled");
    }
  }

  private static void testShaderSources() throws Exception {
    Path root = Files.createTempDirectory("blockbox-shaders-test");
    try {
      Path shaders = Files.createDirectories(root.resolve("shaders/lib"));
      Files.writeString(shaders.resolve("shared.glsl"), "vec3 tint = vec3(1);\n");
      Path entry = root.resolve("shaders/main.fsh");
      Files.writeString(entry, "#version 120\n#include \"/lib/shared.glsl\"\nvoid main() {}\n");
      check(ShaderSources.read(entry, shaders.getParent()).contains("vec3 tint"), "root-relative shader include expands");
      Files.writeString(root.resolve("outside.glsl"), "private data");
      Files.writeString(entry, "#include \"../outside.glsl\"\n");
      expectIOException(() -> ShaderSources.read(entry, shaders.getParent()), "shader traversal rejected");
      Path link = shaders.resolve("link.glsl");
      Files.createSymbolicLink(link, root.resolve("outside.glsl"));
      Files.writeString(entry, "#include \"lib/link.glsl\"\n");
      expectIOException(() -> ShaderSources.read(entry, shaders.getParent()), "shader symlink escape rejected");
      Files.writeString(entry, "#include \"main.fsh\"\n");
      expectIOException(() -> ShaderSources.read(entry, shaders.getParent()), "recursive shader include rejected");
      Files.writeString(entry, "x".repeat(1024*1024+1));
      expectIOException(() -> ShaderSources.read(entry, shaders.getParent()), "oversized shader source rejected");
    } finally {
      BlockboxFiles.deleteRecursive(root);
    }
  }

  private static void testSaveFiles() throws Exception {
    Path root = Files.createTempDirectory("blockbox-saves-test");
    try {
      check("New World".equals(BlockboxSaveFiles.sanitizeWorldName("   ")), "save name sanitizer has fallback");
      check("World (2)".equals(BlockboxSaveFiles.sanitizeWorldName("World (2)")), "save name sanitizer preserves numbered names");
      check("World_One".equals(BlockboxSaveFiles.sanitizeWorldName("World/One")), "save name sanitizer removes path separators");
      check("manual.world".equals(BlockboxSaveFiles.worldDirectory(root, "manual.world").getFileName().toString()), "worldDirectory preserves safe existing names");

      BlockboxSaveFiles.PreparedWorld prepared = BlockboxSaveFiles.prepareWorldForSave(root, "World One");
      check(Files.isDirectory(prepared.worldDirectory()), "prepareWorldForSave creates world directory");
      check(Files.isDirectory(prepared.chunksDirectory()), "prepareWorldForSave creates chunks directory");

      BlockboxSaveFiles.writeWorldDataAtomic(prepared.worldDirectory(), out -> {
        out.writeLong(1234L);
        out.writeUTF("metadata");
      });
      BlockboxSaveFiles.readWorldData(prepared.worldDirectory(), in -> {
        check(in.readLong() == 1234L, "world data long round trip");
        check("metadata".equals(in.readUTF()), "world data UTF round trip");
      });

      BlockboxSaveFiles.writeChunkDataAtomic(prepared.chunksDirectory(), -4, 7, out -> {
        out.writeInt(99);
      });
      boolean loaded = BlockboxSaveFiles.readChunkDataIfPresent(prepared.chunksDirectory(), -4, 7, in -> check(in.readInt() == 99, "chunk data round trip"));
      check(loaded, "readChunkDataIfPresent reports existing chunk");
      boolean missing = BlockboxSaveFiles.readChunkDataIfPresent(prepared.chunksDirectory(), 100, 100, in -> {});
      check(!missing, "readChunkDataIfPresent reports missing chunk");

      check("World One (2)".equals(BlockboxSaveFiles.uniqueWorldName(root, "World One")), "uniqueWorldName appends numbered suffix");
      check(BlockboxSaveFiles.listWorldSaves(root).size() == 1, "listWorldSaves finds valid world");
      BlockboxSaveFiles.writeWorldIndex(root);
      String index = BlockboxFiles.readText(root.resolve(BlockboxSaveFiles.INDEX_FILE), 1024);
      check(index.startsWith("World One|"), "writeWorldIndex records save name and timestamp");
    } finally {
      BlockboxFiles.deleteRecursive(root);
    }
  }

  private static void testSockets() throws Exception {
    expectIllegalArgument(() -> BlockboxSockets.bindIpv4Server(-1), "server bind rejects invalid port");
    expectIllegalArgument(() -> BlockboxSockets.connectTcp("", 25565, 1), "connect rejects blank host");
    expectIllegalArgument(() -> BlockboxSockets.connectTcp("localhost", 70000, 1), "connect rejects invalid port");
    Thread thread = BlockboxSockets.daemonThread("", () -> {});
    check(thread.isDaemon(), "daemonThread creates daemon thread");
    if ("1".equals(System.getenv("BLOCKBOX_SKIP_SOCKET_TESTS"))) {
      System.out.println("Socket bind test skipped: BLOCKBOX_SKIP_SOCKET_TESTS=1");
      return;
    }
    try (ServerSocket server = BlockboxSockets.bindIpv4Server(0)) {
      check(server.isBound(), "bindIpv4Server binds an ephemeral server socket");
    }
  }

  private static void check(boolean condition, String message) {
    if (!condition) throw new AssertionError(message);
  }

  private static void expectIOException(IoRunnable runnable, String message) throws Exception {
    try {
      runnable.run();
      throw new AssertionError(message);
    } catch (IOException expected) {
    }
  }

  private static void expectIllegalArgument(ThrowingRunnable runnable, String message) throws Exception {
    try {
      runnable.run();
      throw new AssertionError(message);
    } catch (IllegalArgumentException expected) {
    }
  }

  @FunctionalInterface private interface IoRunnable { void run() throws Exception; }
  @FunctionalInterface private interface ThrowingRunnable { void run() throws Exception; }
}
