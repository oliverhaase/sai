package misc;

public class CallGraphExample {

    public int bound(int minValue, int value, int maxValue) {
        return max(minValue, min(value, maxValue));
    }

    public int min(int a, int b) {
        log("call min function");
        if (a < b) return a;
        return b;
    }

    public int max(int a, int b) {
        log("call max function");
        if (a > b) return a;
        return b;
    }

    public void log(String s) {
    }

    public void directRecursive() {
        directRecursive();
    }

    // x and y are mutual recursive
    public void x() {
        y();
    }

    public void y() {
        x();
    }

    public int factorial(int n) {
        int result = 0;
        if (n == 0) {
            result = 1;
        } else {
            int tmp1 = n - 1;
            int tmp2 = factorial(tmp1);
            result = n * tmp2;
        }
        return result;
    }

    public boolean isEven(int x) {
        boolean result;
        if (x == 0) {
            result = true;
        } else {
            int tmp = x - 1;
            result = isOdd(tmp);
        }
        return result;
    }

    public boolean isOdd(int x) {
        boolean result;
        if (x == 0) {
            result = false;
        } else {
            int tmp = x - 1;
            result = isEven(tmp);
        }
        return result;
    }

}
