package A;

class A {
  public int test() {
    return 12;
  }

  private int test2() {
    return 13913 + (16 << 1);
  }

  protected A self() {
    return this;
  }
}

class identifier {

  int a;

  boolean test() {

  }

  public static void main(int argc) {
    int a = null;
    int[] array = {1, 12.123};

    array = new int[] {1, 12.123};

    int c = 12 & 293;
    int d = 12 & 293;
    int e = 12.56 + 13.745 / 85.152 * (12 >> 2 & 89);

    if (d) {

      e++;
      e--;
      e += 12;
      e /= 19;
      e *= 39.023;

      int[][] anArray;

      a = (true ? 12 : 14);

      // allocates memory for 10 integers
      anArray = new int[10][20];
    }

    for (int i = 12, j = 18; i != 1289; i++) {
      a *= 12;

      boolean test = (a instanceof identifier);

      int testCast = (int)(123.45);
      if (test()) {
        String a = "Hello ";
        a = (a + "world !");
        return a;
      }
    }

    while (2 < 10) {
      A a;
      a.test();
      a.test2();
    }

    do {
      int d = 12311;
    } while (false);

    try {
      int f;
      f *= 5.0f;
    } catch (Exception e) {
      int coucou;
      coucou += 20;
    } finally {
      int h;
      h -= 50;
    }
  }

}

