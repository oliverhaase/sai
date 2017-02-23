package sai;

public class TestClass {
	public void main(int i, String[] args, long l, Object o) {
		if (args.length < 3)
			System.out.println("false");
		else
			throw new RuntimeException();
	}
}
