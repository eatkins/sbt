package sbt.client;

import sbt.internal.client.SimpleClient;

public class Client {
  public static void main(final String[] args) {
    try {
      SimpleClient.apply(args);
    } catch (final Throwable t) {
      t.printStackTrace();
    }
  }
}
