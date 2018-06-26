package sai;

import java.util.Random;

class T {

    private T f;

    /*
    static T p = new T();
    static T q = new T();
    */

    static void basicStatement1() {
        T p = new T();
        p = new T();
    }

    static void basicStatement2() {
        T p = new T();
        T q = new T();
        p = q;
    }

    static void basicStatement3() {
        T p = new T();
        T q = new T();
        p.f = q;
    }

    static void basicStatement4() {
        T p = new T();
        T q = new T();
        p = q.f;
    }

    static void intraproceduralAnalysisExample() {
        T a = new T();
        T b = a;
        if (new Random().nextBoolean()) {
            a.f = new T();
        } else {
            b.f = new T();
        }
        a = b.f;
    }

}
