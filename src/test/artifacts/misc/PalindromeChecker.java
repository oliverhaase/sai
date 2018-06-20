package misc;

public class PalindromeChecker {
    static boolean isPalindrome(String candidate) {
        int a = 0;
        int b = candidate.length() - 1;
        boolean palindrome = true;
        while (a < b) {
            if (candidate.charAt(a) == candidate.charAt(b)) {
                a++;
                b--;
            } else {
                palindrome = false;
                break;
            }
        }
        return palindrome;
    }
}
