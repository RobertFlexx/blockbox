package blockbox.net;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.SocketException;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.BlockingQueue;

public final class BlockboxSockets {
  public static final int SOCKET_BUFFER_BYTES = 256 * 1024;
  public static final int SERVER_BACKLOG = 64;

  private BlockboxSockets() {}

  public static ServerSocket bindIpv4Server(int port) throws IOException {
    validateBindPort(port);
    ServerSocket server = new ServerSocket();
    boolean bound = false;
    try {
      server.setReuseAddress(true);
      server.bind(new InetSocketAddress("0.0.0.0", port), SERVER_BACKLOG);
      bound = true;
      return server;
    } finally {
      if (!bound) closeQuietly(server);
    }
  }

  public static Socket connectTcp(String host, int port, int timeoutMillis) throws IOException {
    validateHost(host);
    validatePort(port);
    validateTimeout(timeoutMillis, "timeoutMillis");
    Socket socket = new Socket();
    boolean connected = false;
    try {
      configureTcp(socket);
      socket.connect(new InetSocketAddress(host.trim(), port), timeoutMillis);
      connected = true;
      return socket;
    } finally {
      if (!connected) closeQuietly(socket);
    }
  }

  public static Socket connectTcp(String host, int port, int connectTimeoutMillis, int readTimeoutMillis) throws IOException {
    validateTimeout(readTimeoutMillis, "readTimeoutMillis");
    Socket socket = connectTcp(host, port, connectTimeoutMillis);
    socket.setSoTimeout(readTimeoutMillis);
    return socket;
  }

  public static void configureTcp(Socket socket) throws IOException {
    Objects.requireNonNull(socket, "socket");
    socket.setTcpNoDelay(true);
    socket.setKeepAlive(true);
    socket.setReuseAddress(true);
    socket.setReceiveBufferSize(SOCKET_BUFFER_BYTES);
    socket.setSendBufferSize(SOCKET_BUFFER_BYTES);
    socket.setSoLinger(false, 0);
    trySetTrafficClass(socket, 0x10);
  }

  public static BufferedReader utf8Reader(Socket socket) throws IOException {
    Objects.requireNonNull(socket, "socket");
    return new BufferedReader(new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
  }

  public static PrintWriter utf8Writer(Socket socket) throws IOException {
    Objects.requireNonNull(socket, "socket");
    return new PrintWriter(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream(), StandardCharsets.UTF_8)), true);
  }

  public static boolean sendLine(PrintWriter writer, String line) {
    if (writer == null || line == null) return false;
    writer.println(line);
    writer.flush();
    return !writer.checkError();
  }

  public static boolean offerLine(BlockingQueue<String> queue, String line) {
    return queue != null && line != null && queue.offer(line);
  }

  public static Thread daemonThread(String name, Runnable runnable) {
    Objects.requireNonNull(runnable, "runnable");
    String threadName = name == null || name.isBlank() ? "blockbox-worker" : name;
    Thread thread = new Thread(runnable, threadName);
    thread.setDaemon(true);
    return thread;
  }

  public static void closeAllQuietly(Closeable... closeables) {
    if (closeables == null) return;
    for (Closeable closeable : closeables) closeQuietly(closeable);
  }

  public static void closeQuietly(Closeable closeable) {
    if (closeable == null) return;
    try {
      closeable.close();
    } catch (Exception ignored) {
    }
  }

  public static void closeQuietly(Socket socket) {
    if (socket == null) return;
    try {
      socket.close();
    } catch (Exception ignored) {
    }
  }

  public static void closeQuietly(ServerSocket socket) {
    if (socket == null) return;
    try {
      socket.close();
    } catch (Exception ignored) {
    }
  }

  public static String remoteAddress(Socket socket) {
    if (socket == null || socket.getRemoteSocketAddress() == null) return "unknown";
    return socket.getRemoteSocketAddress().toString();
  }

  private static void validateHost(String host) {
    if (host == null || host.trim().isEmpty()) throw new IllegalArgumentException("host is required");
    if (host.indexOf('\0') >= 0) throw new IllegalArgumentException("host contains NUL");
  }

  private static void validatePort(int port) {
    if (port < 1 || port > 65535) throw new IllegalArgumentException("port out of range: " + port);
  }

  private static void validateBindPort(int port) {
    if (port < 0 || port > 65535) throw new IllegalArgumentException("port out of range: " + port);
  }

  private static void validateTimeout(int timeoutMillis, String name) {
    if (timeoutMillis < 0) throw new IllegalArgumentException(name + " must be non-negative");
  }

  private static void trySetTrafficClass(Socket socket, int value) {
    try {
      socket.setTrafficClass(value);
    } catch (SocketException | IllegalArgumentException ignored) {
    }
  }
}
