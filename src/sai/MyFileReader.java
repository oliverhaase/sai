package sai;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;

public class MyFileReader {

    public static void readIntoArray(String filePath, String[] array) throws FileNotFoundException {

        // may throw FileNotFoundException, SecurityException, NullPointerException
        java.io.FileReader fr = new java.io.FileReader(filePath);

        // may throw IllegalArgumentException, NullPointerException
        BufferedReader br = new BufferedReader(fr);

        try {

            // may throw NullPointerException
            final int len = array.length;

            int i = 0;

            // may throw IOException
            String line = br.readLine();

            // may throw NullPointerException, ArrayIndexOutOfBoundsException, ArrayStoreException
            array[i] = line;

            // abstract loop analysis is not supported yet!
            /*while (line != null) {
                // may throw NullPointerException, ArrayIndexOutOfBoundsException, ArrayStoreException
                array[i] = line;
                i++;
                // may throw IOException
                line = br.readLine();
            }*/

            // may throw IOException
            br.close();
        } catch (IndexOutOfBoundsException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
