package sbt.client;

import sbt.internal.client.SimpleClient;

public class Client {
  public static void main(final String[] args) {
    try {
      System.out.println("HUH " + new java.io.File("").getCanonicalFile());
      SimpleClient.apply(args);
    } catch (final Throwable t) {
      t.printStackTrace();
    }
  }
}
