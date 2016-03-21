//public class EmptyTree<T extends Comparable<T>> extends AVLTree<T>
public class EmptyTree<T extends Comparable<T>>
{
    private static AVLTree emptyTree  = null;


    public AVLTree<T> getInstance()
    {
        if( emptyTree == null)
        {
            emptyTree = new AVLTree<T>();
        }
        return emptyTree;
    }
}
