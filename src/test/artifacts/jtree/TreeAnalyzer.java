package jtree;

public final class TreeAnalyzer {

    public static int sum(Tree tree) {
        if (tree == null) {
            return 0;
        }
        final int leftSum = sum(tree.getLeft());
        final int rightSum = sum(tree.getRight());
        final int treeSum = tree.getData() + leftSum + rightSum;
        return treeSum;
    }

    public static boolean isLeaf(Tree tree) {
        return !tree.hasLeft() && !tree.hasRight();
    }

}
