package misc;

import java.util.Random;

public class BasicBlockExamples {

    public void simple() {
        System.out.println("entry");
        System.out.println("...");
        System.out.println("exit");
    }

    public int whileIfElse() {
        int x = 0;
        int y = 0;
        int z = 1;
        while (x < 5) {
            x++;
            if (x >= 2) {
                y = 7;
            } else {
                z = y;
            }
        }
        int result = x+y+z;
        return result;
    }

    public void tryFinally() {
        System.out.println("entry");
        try {
            System.out.println("try-block");
        } finally {
            System.out.println("finally-block");
        }
        System.out.println("exit");
    }

    public void tryCatchFinally() {
        System.out.println("entry");
        try {
            System.out.println("try-block");
        } catch(Exception e) {
            System.out.println("catch-block");
        } finally {
            System.out.println("finally-block");
        }
        System.out.println("exit");
    }

    public void tryCatchCatchFinally() {
        System.out.println("entry");
        try {
            System.out.println("try-block");
        } catch (RuntimeException e) {
            System.out.println("catch-block1");
        } catch(Exception e) {
            System.out.println("catch-block2");
        } finally {
            System.out.println("finally-block");
        }
        System.out.println("exit");
    }

    public void ifStatement() {
        System.out.println("entry");
        if (new Random().nextBoolean()) {
            System.out.println("in if-block");
        }
        System.out.println("exit");
    }

    public void ifElseStatement() {
        System.out.println("entry");
        if (new Random().nextBoolean()) {
            System.out.println("random boolean is true");
        } else {
            System.out.println("random boolean is false");
        }
        System.out.println("exit");
    }

    public void whileLoop() {
        System.out.println("entry");
        int x = 10;
        while (x >= 0) {
            System.out.println(x--);
        }
        System.out.println("exit");
    }

    public void tryCatch() {
        System.out.println("entry");
        try {
            System.out.println("try-block");
        } catch(Exception e) {
            System.out.println("catch-block");
        }
        System.out.println("exit");
    }

    public void emptyMethod() {
    }

    public Object multipleReturns() {
        System.out.println("entry");
        if (new Random().nextBoolean()) {
            System.out.println("in if-block");
            return new Object();
        }
        System.out.println("after if-block");
        return new Object();
    }

    public void ifInFinally() {
        int x = 0;
        try {
            System.out.println("in-try");
            x = new Random().nextInt();
        } finally {
            if (true) {
                System.out.println("passed try block successfully");
            }
        }
        System.out.println(x);
    }

    public void throwsEveryTime() {
        System.out.println("entry");
        throw new RuntimeException();
    }

    public void maybeThrows() {
        if (new Random().nextBoolean()) {
            throw new RuntimeException();
        }
    }

    public void throwInCatch() {
        try {
            System.out.println("try");
        } catch (Throwable e) {
            throw new RuntimeException();
        }
    }

    public void doWhile() {
        System.out.println("entry");
        do {
            System.out.println("body");
        } while (new Random().nextBoolean());
        System.out.println("exit");
    }

}
