public class Node<T extends Comparable<T>>
{
    private T data;

    public Node()
    {
        data = null;
    }

    public Node(T d)
    {
        data = d;
    }

    public boolean isEmpty()
    {
        return data == null;
    }

    public int compareTo( Node<T> other)
    {
        return data.compareTo( other.getData());
    }

    public void setData(T d)
    {
        data = d;
    }

    public T getData()
    {
        return data;
    }
}
