package sai;

class T {

    static T s = null;
    static T s2 = null;

    static class MyRuntimeException extends RuntimeException {
        public T t;
        public String s;
    }

    private T f;

    public void m() {}

     static void createNewObject() {
        T p = new T();
    }

    static void kill() {
        T x = new T();
        T y = x;
        T z = y;
        x = new T();
    }

    static void putstatic() {
        T x = new T();
        s = new T();
        s = x;
    }

    static void getstatic(boolean x) {
        T a = new T();
        T b = new T();
        if (x) {
            a = s;
        } else {
            b = s;
        }
    }

    static void putfield() {
        T x = new T();
        x.f = s;
        x.f = s2;
        x.f = x;
        x.f = new T();
    }

    static void getfield(boolean b) {
        T x = new T();
        T y = new T();
        T z;
        if (b) {
            z = x.f;
        } else {
            z = y.f;
        }
    }

    static T returnObject() {
        T x = s;
        T y = x;
        return y;
    }

    static void throwException() {
        T x = new T();
        MyRuntimeException e = new MyRuntimeException();
        e.t = x;
        try {
            throw e;
        } catch (MyRuntimeException mre) {
            s = mre.t;
        }
    }

    static void intraproceduralAnalysisExample(boolean x) {
        T a = new T();
        T b = a;
        if (/*...*/x) {
            a.f = new T();
        } else {
            b.f = new T();
        }
        a = b.f;
    }

    static T whileLoop(int i) {
        T x = new T();
        while (--i < 10) {
            x.f = new T();
            x = x.f;
        }
        return x;
    }

    static void largeSample(boolean b) {
        T t1 = new T();
        T t2 = null;
        if (b) {
            t2 = new T();
        }

        for (int i = 0; i < 100; i++) {
            if (i % 50 == 0)
                s = new T();
            else if (b && i % 20 == 0)
                t2.f = t1;
        }

        s.f = t2;
    }


    public T loopExample(int x) {
        T a = new T();
        T b = new T();
        T c = a;
        while (x > 0) {
            if (x % 2 == 0) {
                c = a;
            } else {
                c = b;
            }
            x--;
        }
        return c;
    }

    static void doNotBypassLocalVarInTryRange() {
        Object o;
        o = new Object();
        try {
            o = new Object();
        } catch (RuntimeException ignore) {
        }
    }

    static T returnOtherOrNull(T other, boolean b) {
        return b ? other : null;
    }

    void deferArgument(T other) {
        other.f = new T();
        other = this.f;
    }

}
