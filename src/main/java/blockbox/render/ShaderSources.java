package blockbox.render;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Bounded shader include expansion with canonical-path sandbox checks. */
public final class ShaderSources {
  private static final int MAX_EXPANDED_CHARS = 4 * 1024 * 1024;
  private static final Pattern INCLUDE =
      Pattern.compile("^#include\\s+[\"<]([^\">]+)[\">]\\s*(?://.*)?$");

  private ShaderSources() {}

  public static String read(Path file, Path sandbox) throws IOException {
    StringBuilder result = new StringBuilder();
    expand(file, sandbox.toRealPath(), new HashSet<>(), new int[] {0}, result);
    return result.toString();
  }

  private static void expand(Path file, Path root, Set<Path> stack, int[] files, StringBuilder out)
      throws IOException {
    Path path = file.toRealPath();
    if (!path.startsWith(root))
      throw new IOException("Shader include escapes pack: " + file);
    if (stack.size() >= 32 || ++files[0] > 512)
      throw new IOException("Shader include limit exceeded");
    if (!stack.add(path))
      throw new IOException("Recursive shader include: " + file);
    if (Files.size(path) > 1024 * 1024)
      throw new IOException("Shader source exceeds 1 MiB: " + file);
    try {
      for (String line : Files.readAllLines(path, StandardCharsets.UTF_8)) {
        String trimmed = line.trim();
        if (trimmed.startsWith("#include")) {
          Matcher match = INCLUDE.matcher(trimmed);
          if (!match.matches())
            throw new IOException("Malformed shader include: " + line);
          String relative = match.group(1);
          Path included = relative.startsWith("/") ? root.resolve(relative.substring(1))
                                                   : path.getParent().resolve(relative);
          expand(included, root, stack, files, out);
        } else {
          if (out.length() + line.length() + 1 > MAX_EXPANDED_CHARS)
            throw new IOException("Expanded shader exceeds 4 MiB");
          out.append(line).append('\n');
        }
      }
    } finally {
      stack.remove(path);
    }
  }
}
