package sbt.client;

import java.io.File;
import sbt.internal.client.SimpleClient;
import scala.collection.immutable.List$;

public class Client {
  public static void main(final String[] args) {
    try {
      final File file =
          args.length == 0 ? new File("").getCanonicalFile() : new File(args[0]).getCanonicalFile();
      new SimpleClient(file, List$.MODULE$.empty());
    } catch (final Throwable t) {
      t.printStackTrace();
    }
  }
}
