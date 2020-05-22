package sbt.client;

import sbt.internal.client.SimpleClient;

public class Client {
  public static void main(final String[] args) {
    try {
      JAnsi.install();
      SimpleClient.apply(args);
    } catch (final Throwable t) {
      t.printStackTrace();
    }
    JAnsi.uninstall();
  }
}
