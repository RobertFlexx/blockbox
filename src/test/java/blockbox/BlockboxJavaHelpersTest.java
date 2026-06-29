package blockbox;
// this is stupid helper me made, feel free to use and test
import blockbox.io.BlockboxFiles;
import blockbox.io.BlockboxSaveFiles;
import blockbox.net.BlockboxNet;
import blockbox.net.BlockboxSockets;

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
    String escaped = BlockboxNet.escape("a|b%c\n");
    check("a|b%c ".equals(BlockboxNet.unescape(escaped)), "escape/unescape round trip for protocol separators");

    String pos = BlockboxNet.pos("Bad Name!", 1.25f, 2f, 3f, 90f, -10f, 99, 8);
    BlockboxNet.Packet packet = BlockboxNet.packet(pos);
    BlockboxNet.PositionPacket parsed = BlockboxNet.parsePosition(packet, 8);
    check("BadName".equals(parsed.name), "position parser sanitizes player name");
    check(parsed.color == 3, "position parser normalizes color");

    BlockboxNet.BlockPacket block = BlockboxNet.parseBlock(BlockboxNet.packet(BlockboxNet.block(1, 2, 3, 4, 8)), 64);
    check(block.x == 1 && block.y == 2 && block.z == 3 && block.blockId == 4 && block.waterLevel == 8, "block parser accepts water level");
    expectIllegalArgument(() -> BlockboxNet.parseBlock(BlockboxNet.packet(BlockboxNet.block(1, 64, 3, 4)), 64), "block parser bounds y");
    expectIllegalArgument(() -> BlockboxNet.packet("bad|packet"), "packet rejects bad type");

    String line = BlockboxNet.readProtocolLine(new BufferedReader(new StringReader("PING\r\nNEXT\n")));
    check("PING".equals(line), "bounded protocol reader handles CRLF");
    expectIOException(() -> BlockboxNet.readProtocolLine(new BufferedReader(new StringReader("A".repeat(BlockboxNet.MAX_LINE_LENGTH + 1) + "\n"))), "bounded protocol reader rejects long line");
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
