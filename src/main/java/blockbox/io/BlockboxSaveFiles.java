package blockbox.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public final class BlockboxSaveFiles {
  public static final String WORLDS_DIR = "worlds";
  public static final String WORLD_DATA_FILE = "world.dat";
  public static final String CHUNKS_DIR = "chunks";
  public static final String INDEX_FILE = "index.txt";
  public static final int MAX_WORLD_NAME_LENGTH = 40;
  public static final int MAX_CHUNK_COORD_TEXT_LENGTH = 16;

  private BlockboxSaveFiles() {}

  public static Path defaultWorldsRoot() {
    return Path.of(WORLDS_DIR).toAbsolutePath().normalize();
  }

  public static String sanitizeWorldName(String raw) {
    String input = raw == null ? "" : raw.trim();
    StringBuilder out = new StringBuilder(MAX_WORLD_NAME_LENGTH);
    boolean pendingSpace = false;
    for (int i = 0; i < input.length() && out.length() < MAX_WORLD_NAME_LENGTH; i++) {
      char ch = input.charAt(i);
      if (Character.isWhitespace(ch)) {
        pendingSpace = out.length() > 0;
      } else {
        if (pendingSpace && out.length() < MAX_WORLD_NAME_LENGTH) out.append(' ');
        pendingSpace = false;
        if (Character.isLetterOrDigit(ch) || ch == '_' || ch == '-' || ch == '(' || ch == ')') out.append(ch);
        else out.append('_');
      }
    }
    String clean = out.toString().trim();
    return clean.isEmpty() ? "New World" : clean;
  }

  public static Path worldDirectory(Path worldsRoot, String worldName) throws IOException {
    Objects.requireNonNull(worldsRoot, "worldsRoot");
    return BlockboxFiles.safeChild(worldsRoot, safeWorldDirectoryName(worldName));
  }

  public static Path chunksDirectory(Path worldDirectory) throws IOException {
    Objects.requireNonNull(worldDirectory, "worldDirectory");
    return BlockboxFiles.safeChild(worldDirectory, CHUNKS_DIR);
  }

  public static Path worldDataFile(Path worldDirectory) throws IOException {
    Objects.requireNonNull(worldDirectory, "worldDirectory");
    return BlockboxFiles.safeChild(worldDirectory, WORLD_DATA_FILE);
  }

  public static Path worldIndexFile(Path worldsRoot) throws IOException {
    Objects.requireNonNull(worldsRoot, "worldsRoot");
    return BlockboxFiles.safeChild(worldsRoot, INDEX_FILE);
  }

  public static Path chunkFile(Path chunksDirectory, int chunkX, int chunkZ) throws IOException {
    Objects.requireNonNull(chunksDirectory, "chunksDirectory");
    return BlockboxFiles.safeChild(chunksDirectory, chunkFileName(chunkX, chunkZ));
  }

  public static String chunkFileName(int chunkX, int chunkZ) {
    String x = Integer.toString(chunkX);
    String z = Integer.toString(chunkZ);
    if (x.length() > MAX_CHUNK_COORD_TEXT_LENGTH || z.length() > MAX_CHUNK_COORD_TEXT_LENGTH) {
      throw new IllegalArgumentException("chunk coordinate text too long");
    }
    return "chunk_" + x + "_" + z + ".dat";
  }

  public static String uniqueWorldName(Path worldsRoot, String requestedName) throws IOException {
    String base = sanitizeWorldName(requestedName);
    String candidate = base;
    int n = 2;
    while (Files.exists(worldDirectory(worldsRoot, candidate), LinkOption.NOFOLLOW_LINKS)) {
      String suffix = " (" + n + ")";
      int maxBase = Math.max(1, MAX_WORLD_NAME_LENGTH - suffix.length());
      candidate = base.substring(0, Math.min(base.length(), maxBase)).trim() + suffix;
      n++;
      if (n < 0) throw new IOException("too many worlds named like: " + base);
    }
    return candidate;
  }

  public static PreparedWorld prepareWorldForSave(Path worldsRoot, String worldName) throws IOException {
    Path root = BlockboxFiles.ensureDirectory(worldsRoot);
    Path world = worldDirectory(root, worldName);
    Path chunks = chunksDirectory(world);
    BlockboxFiles.ensureDirectory(world);
    BlockboxFiles.ensureDirectory(chunks);
    return new PreparedWorld(root, world, chunks, worldDataFile(world));
  }

  public static PreparedWorld prepareWorldForSave(Path worldDirectory) throws IOException {
    Objects.requireNonNull(worldDirectory, "worldDirectory");
    Path world = BlockboxFiles.ensureDirectory(worldDirectory);
    Path root = world.getParent() == null ? Path.of(".").toAbsolutePath().normalize() : world.getParent().toAbsolutePath().normalize();
    Path chunks = chunksDirectory(world);
    BlockboxFiles.ensureDirectory(chunks);
    return new PreparedWorld(root, world, chunks, worldDataFile(world));
  }

  public static void writeWorldDataAtomic(Path worldDirectory, DataWriter writer) throws IOException {
    Objects.requireNonNull(writer, "writer");
    PreparedWorld prepared = prepareWorldForSave(worldDirectory);
    writeDataAtomic(prepared.worldDataFile(), writer);
  }

  public static void readWorldData(Path worldDirectory, DataReader reader) throws IOException {
    Objects.requireNonNull(reader, "reader");
    Path file = worldDataFile(worldDirectory.toAbsolutePath().normalize());
    if (!Files.isRegularFile(file)) throw new IOException("missing world data: " + file);
    try (DataInputStream in = new DataInputStream(new BufferedInputStream(Files.newInputStream(file)))) {
      reader.read(in);
    }
  }

  public static void writeChunkDataAtomic(Path chunksDirectory, int chunkX, int chunkZ, DataWriter writer) throws IOException {
    Objects.requireNonNull(writer, "writer");
    Path dir = BlockboxFiles.ensureDirectory(chunksDirectory);
    writeDataAtomic(chunkFile(dir, chunkX, chunkZ), writer);
  }

  public static boolean readChunkDataIfPresent(Path chunksDirectory, int chunkX, int chunkZ, DataReader reader) throws IOException {
    Objects.requireNonNull(reader, "reader");
    Path file = chunkFile(chunksDirectory.toAbsolutePath().normalize(), chunkX, chunkZ);
    if (!Files.exists(file)) return false;
    if (!Files.isRegularFile(file)) throw new IOException("chunk data is not a regular file: " + file);
    try (DataInputStream in = new DataInputStream(new BufferedInputStream(Files.newInputStream(file)))) {
      reader.read(in);
    }
    return true;
  }

  public static List<WorldSave> listWorldSaves(Path worldsRoot) throws IOException {
    Objects.requireNonNull(worldsRoot, "worldsRoot");
    Path root = worldsRoot.toAbsolutePath().normalize();
    if (!Files.isDirectory(root)) return List.of();
    List<WorldSave> saves = new ArrayList<>();
    try (var stream = Files.list(root)) {
      stream.forEach(path -> {
        try {
          Path normalized = path.toAbsolutePath().normalize();
          Path worldData = worldDataFile(normalized);
          if (Files.isDirectory(normalized) && Files.isRegularFile(worldData)) {
            saves.add(new WorldSave(normalized, worldData, Files.getLastModifiedTime(worldData).toMillis()));
          }
        } catch (IOException ignored) {
        }
      });
    }
    saves.sort(Comparator.comparingLong(WorldSave::lastModifiedMillis).reversed().thenComparing(save -> save.name().toLowerCase(java.util.Locale.ROOT)));
    return Collections.unmodifiableList(saves);
  }

  public static void writeWorldIndex(Path worldsRoot) throws IOException {
    Path root = BlockboxFiles.ensureDirectory(worldsRoot);
    StringBuilder text = new StringBuilder();
    for (WorldSave save : listWorldSaves(root)) {
      text.append(save.name()).append('|').append(save.lastModifiedMillis()).append('\n');
    }
    BlockboxFiles.writeTextAtomic(worldIndexFile(root), text.toString());
  }

  private static void writeDataAtomic(Path file, DataWriter writer) throws IOException {
    BlockboxFiles.writeAtomic(file, raw -> {
      try {
        DataOutputStream out = new DataOutputStream(new BufferedOutputStream(raw));
        writer.write(out);
        out.flush();
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    });
  }

  private static String safeWorldDirectoryName(String worldName) {
    String raw = worldName == null ? "" : worldName.trim();
    if (raw.isEmpty() || raw.indexOf('\0') >= 0 || raw.indexOf('/') >= 0 || raw.indexOf('\\') >= 0 || ".".equals(raw) || "..".equals(raw) || Path.of(raw).isAbsolute()) {
      return sanitizeWorldName(raw);
    }
    return raw;
  }

  public record PreparedWorld(Path worldsRoot, Path worldDirectory, Path chunksDirectory, Path worldDataFile) {}

  public record WorldSave(Path directory, Path worldDataFile, long lastModifiedMillis) {
    public String name() {
      Path fileName = directory.getFileName();
      return fileName == null ? directory.toString() : fileName.toString();
    }
  }

  @FunctionalInterface public interface DataWriter { void write(DataOutputStream out) throws IOException; }
  @FunctionalInterface public interface DataReader { void read(DataInputStream in) throws IOException; }
}
