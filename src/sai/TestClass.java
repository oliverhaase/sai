package sai;

public class TestClass {
	public static void main(String[] args) {
		if (args.length < 3)
			System.out.println("false");
		else
			throw new RuntimeException();
	}
}
