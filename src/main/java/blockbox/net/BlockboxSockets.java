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
import java.util.concurrent.BlockingQueue;

public final class BlockboxSockets {
  public static final int SOCKET_BUFFER_BYTES = 256 * 1024;
  public static final int SERVER_BACKLOG = 64;

  private BlockboxSockets() {}

  public static ServerSocket bindIpv4Server(int port) throws IOException {
    ServerSocket server = new ServerSocket();
    server.setReuseAddress(true);
    server.bind(new InetSocketAddress("0.0.0.0", port), SERVER_BACKLOG);
    return server;
  }

  public static Socket connectTcp(String host, int port, int timeoutMillis) throws IOException {
    Socket socket = new Socket();
    configureTcp(socket);
    socket.connect(new InetSocketAddress(host, port), timeoutMillis);
    return socket;
  }

  public static void configureTcp(Socket socket) throws IOException {
    socket.setTcpNoDelay(true);
    socket.setKeepAlive(true);
    socket.setReuseAddress(true);
    socket.setReceiveBufferSize(SOCKET_BUFFER_BYTES);
    socket.setSendBufferSize(SOCKET_BUFFER_BYTES);
    socket.setSoLinger(false, 0);
    trySetTrafficClass(socket, 0x10);
  }

  public static BufferedReader utf8Reader(Socket socket) throws IOException {
    return new BufferedReader(new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
  }

  public static PrintWriter utf8Writer(Socket socket) throws IOException {
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
    Thread thread = new Thread(runnable, name);
    thread.setDaemon(true);
    return thread;
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

  private static void trySetTrafficClass(Socket socket, int value) {
    try {
      socket.setTrafficClass(value);
    } catch (SocketException | IllegalArgumentException ignored) {
    }
  }
}
