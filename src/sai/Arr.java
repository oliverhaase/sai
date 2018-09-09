package sai;

class Arr {

    Arr f;
    static Arr s;

    static void createArray() {
        Arr[] array = new Arr[5];
        array[0] = new Arr();
        array[9] = new Arr();

        Arr x = new Arr();
        array[0] = x;
    }

    static void multidimensionalArray() {
        Arr[][][] arr = new Arr[10][20][30];

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                for (int k = 0; k < 10; k++) {
                    Arr myArr = new Arr();
                    arr[i][j][k] = myArr;
                }
            }
        }

    }

    static void example(Arr[] otherArray, Arr other, int index) {
        Arr[] b = otherArray;
        otherArray[2] = new Arr();
        b[++index] = other;
    }

    static void intArray() {
        int[][] a = new int[10][20];
        a[0][0] = 20;
    }

    static void aaload() {
        Arr[] arr = new Arr[20];
        arr[0] = new Arr();
        Arr x = arr[0];
        Arr y = arr[1];
    }

    public static void main(String[] args) {
        multidimensionalArray();
    }

}
