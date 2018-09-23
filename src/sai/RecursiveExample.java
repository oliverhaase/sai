package sai;

public class RecursiveExample {

    static RecursiveExample a;
    static RecursiveExample b;
    static RecursiveExample c;
    static RecursiveExample d;
    static RecursiveExample e;

    static RecursiveExample f1;
    static RecursiveExample f2;
    static RecursiveExample f3;

    static RecursiveExample e1;
    static RecursiveExample e2;
    static RecursiveExample e3;

    static RecursiveExample o1;
    static RecursiveExample o2;
    static RecursiveExample o3;


    static void a() {
        a = new RecursiveExample();
        b();
    }

    static void b() {
        b = new RecursiveExample();
        c();
    }

    static void c() {
        c = new RecursiveExample();
        d();
        if (bool) {
            a();
        }
    }

    static void d() {
        d = new RecursiveExample();
        e();
    }

    static void e() {
        if (bool) {
            e = new RecursiveExample();
            isEven(5);
        } else {
            e = new RecursiveExample();
            factorial(5);
        }
    }

    static void factorial(int x) {
        if (x == 0) {
            f1 = new RecursiveExample();
        } else {
            f2 = new RecursiveExample();
            factorial(x - 1);
            f3 = new RecursiveExample();
        }
    }

    static void isEven(int x) {
        if (x == 0) {
            e1 = new RecursiveExample();
        } else {
            e2 = new RecursiveExample();
            isOdd(x-1);
            e3 = new RecursiveExample();
        }
    }

    static void isOdd(int x) {
        if (x == 1) {
            o1 = new RecursiveExample();
        } else {
            o2 = new RecursiveExample();
            isEven(x-1);
            o3 = new RecursiveExample();
        }
    }

    static boolean bool = true;

}
