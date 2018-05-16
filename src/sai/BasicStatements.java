package sai;

public class BasicStatements {

    static class T {
        int i;
        T f;
    }

    public static void localAssignment() {
        T localT = new T();
        localT.f = new T();
    }

    private static T staticT = new T();
    public static void staticAssignment() {
        staticT.f = new T();
    }

    private T instanceT = new T();
    public void instanceAssignment() {
        instanceT.f = new T();
    }

    public void localDefer() {
        T t = instanceT;
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

}
