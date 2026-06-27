package blockbox.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public final class BlockboxFiles {
  public static final long DEFAULT_MAX_TEXT_BYTES = 4L * 1024L * 1024L;
  private static final int HASH_BUFFER_BYTES = 64 * 1024;

  private BlockboxFiles() {}

  public static Path ensureDirectory(Path dir) throws IOException {
    Objects.requireNonNull(dir, "dir");
    return Files.createDirectories(dir);
  }

  public static Path safeChild(Path root, String child) throws IOException {
    Objects.requireNonNull(root, "root");
    String clean = child == null ? "" : child.replace('\\', '/');
    if (clean.isBlank() || clean.startsWith("/") || clean.contains("\u0000")) {
      throw new IOException("unsafe path: " + clean);
    }
    Path normalizedRoot = root.toAbsolutePath().normalize();
    Path resolved = normalizedRoot.resolve(clean).normalize();
    if (!resolved.startsWith(normalizedRoot)) {
      throw new IOException("path escapes root: " + clean);
    }
    return resolved;
  }

  public static String safeName(String value, String fallback, int maxLength) {
    String input = value == null ? "" : value.trim();
    StringBuilder out = new StringBuilder(Math.max(8, Math.min(64, maxLength)));
    for (int i = 0; i < input.length() && out.length() < maxLength; i++) {
      char ch = input.charAt(i);
      if (Character.isLetterOrDigit(ch) || ch == '_' || ch == '-' || ch == '.' || ch == ' ') out.append(ch);
    }
    String clean = out.toString().trim();
    return clean.isEmpty() ? (fallback == null || fallback.isBlank() ? "file" : fallback) : clean;
  }

  public static String readText(Path file, String defaultValue) {
    try {
      return readText(file, DEFAULT_MAX_TEXT_BYTES);
    } catch (Exception ignored) {
      return defaultValue == null ? "" : defaultValue;
    }
  }

  public static String readText(Path file, long maxBytes) throws IOException {
    Objects.requireNonNull(file, "file");
    if (!Files.isRegularFile(file)) throw new IOException("not a regular file: " + file);
    long size = Files.size(file);
    if (size > maxBytes) throw new IOException("file too large: " + file + " (" + size + " bytes)");
    return Files.readString(file, StandardCharsets.UTF_8);
  }

  public static void writeTextAtomic(Path file, String text) throws IOException {
    byte[] bytes = (text == null ? "" : text).getBytes(StandardCharsets.UTF_8);
    writeAtomic(file, out -> {
      try {
        out.write(bytes);
      } catch (IOException e) {
        throw new UncheckedIo(e);
      }
    });
  }

  public static void writeAtomic(Path file, Consumer<java.io.OutputStream> writer) throws IOException {
    Objects.requireNonNull(file, "file");
    Objects.requireNonNull(writer, "writer");
    Path target = file.toAbsolutePath().normalize();
    Path parent = target.getParent();
    if (parent != null) Files.createDirectories(parent);
    String prefix = target.getFileName() == null ? "bbx" : target.getFileName().toString();
    if (prefix.length() < 3) prefix = "bbx" + prefix;
    Path tmp = parent == null ? Files.createTempFile(prefix, ".tmp") : Files.createTempFile(parent, prefix, ".tmp");
    boolean moved = false;
    try {
      try (java.io.OutputStream out = Files.newOutputStream(tmp, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)) {
        try {
          writer.accept(out);
        } catch (UncheckedIo e) {
          throw e.cause;
        }
      }
      moveReplace(tmp, target);
      moved = true;
      fsyncParent(target);
    } finally {
      if (!moved) Files.deleteIfExists(tmp);
    }
  }

  public static void moveReplace(Path source, Path target) throws IOException {
    try {
      Files.move(source, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
    } catch (AtomicMoveNotSupportedException e) {
      Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
    }
  }

  public static String sha256Bytes(byte[] bytes) {
    MessageDigest md = sha256Digest();
    md.update(bytes == null ? new byte[0] : bytes);
    return HexFormat.of().formatHex(md.digest());
  }

  public static String sha256File(Path file) throws IOException {
    MessageDigest md = sha256Digest();
    byte[] buffer = new byte[HASH_BUFFER_BYTES];
    try (InputStream in = new BufferedInputStream(Files.newInputStream(file))) {
      int n;
      while ((n = in.read(buffer)) >= 0) {
        if (n > 0) md.update(buffer, 0, n);
      }
    }
    return HexFormat.of().formatHex(md.digest());
  }

  public static String sha256Tree(Path root) throws IOException {
    Path normalized = root.toAbsolutePath().normalize();
    if (Files.isRegularFile(normalized)) return sha256File(normalized);
    MessageDigest md = sha256Digest();
    List<Path> files = new ArrayList<>();
    if (Files.isDirectory(normalized)) {
      Files.walkFileTree(normalized, new SimpleFileVisitor<>() {
        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
          if (attrs.isRegularFile()) files.add(file.toAbsolutePath().normalize());
          return FileVisitResult.CONTINUE;
        }
      });
    }
    files.sort(Comparator.comparing(p -> normalized.relativize(p).toString().replace('\\', '/')));
    for (Path file : files) {
      String rel = normalized.relativize(file).toString().replace('\\', '/');
      md.update(rel.getBytes(StandardCharsets.UTF_8));
      md.update((byte) 0);
      md.update(sha256File(file).getBytes(StandardCharsets.UTF_8));
      md.update((byte) '\n');
    }
    return HexFormat.of().formatHex(md.digest());
  }

  public static void deleteRecursive(Path root) throws IOException {
    if (root == null || !Files.exists(root)) return;
    Files.walkFileTree(root, new SimpleFileVisitor<>() {
      @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        Files.deleteIfExists(file);
        return FileVisitResult.CONTINUE;
      }

      @Override public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
        if (exc != null) throw exc;
        Files.deleteIfExists(dir);
        return FileVisitResult.CONTINUE;
      }
    });
  }

  public static void copyRecursive(Path source, Path target) throws IOException {
    if (Files.isDirectory(source)) {
      Files.walkFileTree(source, new SimpleFileVisitor<>() {
        @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
          Files.createDirectories(target.resolve(source.relativize(dir).toString()));
          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
          Path out = target.resolve(source.relativize(file).toString());
          Files.createDirectories(out.getParent());
          Files.copy(file, out, StandardCopyOption.REPLACE_EXISTING);
          return FileVisitResult.CONTINUE;
        }
      });
    } else {
      Files.createDirectories(target.getParent());
      Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
    }
  }

  private static MessageDigest sha256Digest() {
    try {
      return MessageDigest.getInstance("SHA-256");
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 is unavailable", e);
    }
  }

  private static void fsyncParent(Path target) {
    Path parent = target.getParent();
    if (parent == null) return;
    try (java.nio.channels.FileChannel channel = java.nio.channels.FileChannel.open(parent, StandardOpenOption.READ)) {
      channel.force(true);
    } catch (Exception ignored) {
    }
  }

  private static final class UncheckedIo extends RuntimeException {
    final IOException cause;
    UncheckedIo(IOException cause) {
      super(cause);
      this.cause = cause;
    }
  }
}
