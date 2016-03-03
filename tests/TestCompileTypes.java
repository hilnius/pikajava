package A;

class A {
  int aMember;

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

class B extends A {

}

class identifier extends A {

  int a;

  private static final int st;

  static {
    st = 5;
  }

  {
    a = 123;
  }

  boolean test(float a, double st) {
    st = 12;

    A ao;
    B bo;

    ao = bo;
    bo = ao;

    return a + 14;
  }

  public static void main(int argc) {
    aMember = 155;
    self();
    self().aMember = 12;

    int a = null;
    int[] array = {1, 12.123};

    array = new int[] {1, 12.123};

    st += "coucou";

    int c = 12 & 293;
    int d = 12 & 293 + 56;
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
      identifier myId = new identifier();
      A castTest = (A)(myId);
      ((A)(myId)).test();
      boolean test = (a instanceof identifier);

      int testCast = (int)(123.45);
      if (test()) {
        String a = "Hello ";
        a = a + "world !";
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

