package sai;

public class GraphExamples {

    GraphExamples f;
    static GraphExamples s;

    // 2.)
    public GraphExamples returnSomething(GraphExamples other, boolean b) {
        GraphExamples result;
        if (b) {
            result = new GraphExamples();
            result.f = this;
        } else {
            result = other.f;
        }
        return result;
    }

    // 1.)
    public void throwException() {
        MyRuntimeException exception = new MyRuntimeException();
        try {
            exception.x = s;
            throw exception;
        } catch(MyRuntimeException m) {
            f = m.x;
        }
    }


    // 3.)
    public GraphExamples fieldExample(GraphExamples other, boolean b) {
        this.f = other;
        if (b) {
            s = other.f;
        } else {
            s = this;
        }
        return s;
    }

    class MyRuntimeException extends RuntimeException {
        GraphExamples x;
    }
}
