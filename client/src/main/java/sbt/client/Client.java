package sbt.client;

import sbt.internal.client.SimpleClient;
import java.nio.file.Paths;
import org.fusesource.jansi.AnsiConsole;

public class Client {
  public static void main(final String[] args) {
    boolean isWin = System.getProperty("os.name").toLowerCase().startsWith("win");
    try {
      if (isWin) AnsiConsole.systemInstall();
      SimpleClient.main(false, args);
    } catch (final Throwable t) {
      t.printStackTrace();
    } finally {
      if (isWin) AnsiConsole.systemUninstall();
    }
  }
}
