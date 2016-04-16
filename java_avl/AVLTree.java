import java.lang.Math;

public class AVLTree<T extends Comparable<T>>
{
    private Node<T> root;
    private AVLTree<T> leftChild;
    private AVLTree<T> rightChild;
    private int height;

    public AVLTree()
    {
        root = null;
        leftChild = null;
        rightChild = null;
        height = 0;
    }

    public AVLTree(T r)
    {
        root        = new Node<T>(r);
        leftChild   = new AVLTree<T>();
        rightChild  = new AVLTree<T>();
        height      = 1;
    }

    public boolean isEmpty()
    {
        return root == null;
    }

    public void display()
    {
        if(! this.isEmpty())
        {
            leftChild.display();
            System.out.println(root.getData());
            rightChild.display();
        }
    }

    public void displayStruct()
    {
        System.out.println("-------------------------");
        this.displayStructRec(0);
        System.out.println("-------------------------");
    }

    private void displayStructRec(int k)
    {
        if(! this.isEmpty())
        {
            this.getRightChild().displayStructRec(k+1);
        }
            for(int i = 0; i < 5*k; i++)
                System.out.print(" ");
        System.out.println(this.getData());
        if(! this.isEmpty())
        {
            this.getLeftChild().displayStructRec(k+1);
        }
    }

    public boolean research(T k)
    {
        if(this.isEmpty())
        {
            return false;
        }
        else if(root.getData().compareTo(k) > 0)
        {
            return leftChild.research(k);
        }
        else if(root.getData().compareTo(k) < 0)
        {
            return rightChild.research(k);
        }
        return true;
    }

    public void add(T k)
    {
        if(this.isEmpty())
        {
            this.insertEmpty(k);
        }
        else if(root.getData().compareTo(k) > 0)
        {
            leftChild.add(k);
            this.equilibrate();
        }
        else if(root.getData().compareTo(k) < 0)
        {
            rightChild.add(k);
            this.equilibrate();
        }
    }

    public void insertEmpty(T k)
    {
        root = new Node<T>(k);
        leftChild =  new AVLTree<T>();
        rightChild =  new AVLTree<T>();
        height = 1;
    }

    public void equilibrate()
    {
        int bal = this.balance();

        if(bal == 2)
        {
            if(rightChild.balance() >= 0)
            {
                this.leftRotation();
            }
            else
            {
                rightChild.rightRotation();
                this.leftRotation();
            }
        }
        else
        {
            if(bal == -2)
            {
                if(leftChild.balance() <= 0)
                {
                    this.rightRotation();
                }
                else
                {
                    leftChild.leftRotation();
                    this.rightRotation();
                }
            }
            else
            {
                this.height();
            }
        }
    }

    public int balance()
    {
        int bal;

        if(this.isLeaf())
        {
            bal = 0;
        }
        else if(leftChild.isEmpty())
        {
            bal = rightChild.getHeight();
        }
        else if(rightChild.isEmpty())
        {
            bal = - leftChild.getHeight();
        }
        else
        {
            bal = rightChild.getHeight() - leftChild.getHeight();
        }
        return bal;
    }

    public void height()
    {
        if(this.isLeaf())
        {
            this.setHeight(1);
        }
        else if(leftChild.isEmpty())
        {
            this.setHeight(1 + rightChild.getHeight());
        }
        else if(rightChild.isEmpty())
        {
            this.setHeight(1 + leftChild.getHeight());
        }
        else
        {
            this.setHeight(1 + Math.max(leftChild.getHeight(),
                                          rightChild.getHeight()));
        }
    }

    public void leftRotation()
    {
        AVLTree<T> temp = new AVLTree<T>();
        temp.copy(this);
        this.copy(rightChild);
        temp.getRightChild().copy(leftChild);
        leftChild.copy(temp);
        temp.height();
        this.height();
    }

    public void rightRotation()
    {
        AVLTree<T> temp = new AVLTree<T>();
        temp.copy(this);
        this.copy(leftChild);
        temp.getLeftChild().copy(rightChild);
        rightChild.copy(temp);
        temp.height();
        this.height();
    }

    public boolean isLeaf()
    {
        return (! this.isEmpty()) &&
                leftChild.isEmpty() &&
                rightChild.isEmpty();
    }

    public void delete(T k)
    {
        if(! this.isEmpty())
        {
            if(root.getData().compareTo(k) > 0)
            {
                leftChild.delete(k);
                this.equilibrate();
            }
            else if(root.getData().compareTo(k) < 0)
            {
                rightChild.delete(k);
                this.equilibrate();
            }
            else
            {
                this.deleteRoot();
            }
        }
    }

    public void deleteRoot()
    {
        if(this.isLeaf())
        {
            root = null;
            leftChild = null;
            rightChild = null;
            height = 0;
        }
        else if(leftChild.isEmpty())
        {
            this.copy(rightChild);
        }
        else if(rightChild.isEmpty())
        {
            this.copy(leftChild);
        }
        else
        {
            root = new Node<T>(rightChild.deleteMin());
            this.equilibrate();
        }
    }

    public T deleteMin()
    {
        T min;
        if(leftChild.isEmpty())
        {
            min = this.getData();
            this.copy(rightChild);
        }
        else
        {
            min = leftChild.deleteMin();
            this.equilibrate();
        }
        return min;
    }

    // on suppose que l'arbre est non vide
    public T findMin()
    {
        if(leftChild.isEmpty())
        {
            return this.getData();
        }
        else
        {
            return leftChild.findMin();
        }
    }

    public T findMax()
    {
        if(rightChild.isEmpty())
        {
            return this.getData();
        }
        else
        {
            return rightChild.findMax();
        }
    }

    public T leftNeighbor(T x)
    {
        return this.leftNeighborRec(x, this.findMin());
    }

    private T leftNeighborRec(T x, T tmp)
    {
        if(this.isEmpty())
        {
            return tmp;
        }
        else if(this.getData().compareTo(x) == 0)
        {
            if(!leftChild.isEmpty())
            {
                return leftChild.findMax();
            }
            else
            {
                return tmp;
            }
        }
        else if(this.getData().compareTo(x) > 0)
        {
            return leftChild.leftNeighborRec(x, tmp);
        }
        else
        {
            if(this.getData().compareTo(tmp) > 0)
            {
                tmp = this.getData();
            }
            if(rightChild.isEmpty())
            {
                return this.getData();
            }
            else if(rightChild.getData().compareTo(x) > 0)
            {
                if(rightChild.getLeftChild().isEmpty())
                {
                    return this.getData();
                }
                else
                {
                    return rightChild.leftNeighborRec(x, tmp);
                }
            }
            else
            {
                return rightChild.leftNeighborRec(x, tmp);
            }
        }
    }

    public T rightNeighbor(T x)
    {
        return this.rightNeighborRec(x, this.findMax());
    }

    private T rightNeighborRec(T x, T tmp)
    {
        if(this.isEmpty())
        {
            return tmp;
        }
        else if(this.getData().compareTo(x) == 0)
        {
            if(!rightChild.isEmpty())
            {
                return rightChild.findMin();
            }
            else
            {
                return tmp;
            }
        }
        else if(this.getData().compareTo(x) < 0)
        {
            return rightChild.rightNeighborRec(x, tmp);
        }
        else
        {
            if(this.getData().compareTo(tmp) < 0)
            {
                tmp = this.getData();
            }
            if(leftChild.isEmpty())
            {
                return this.getData();
            }
            else if(leftChild.getData().compareTo(x) < 0)
            {
                if(leftChild.getRightChild().isEmpty())
                {
                    return this.getData();
                }
                else
                {
                    return leftChild.rightNeighborRec(x, tmp);
                }
            }
            else
            {
                return leftChild.rightNeighborRec(x, tmp);
            }
        }
    }

    public void copy(AVLTree<T> tree)
    {
        int h = tree.getHeight();
        root = tree.getRoot();
        leftChild = tree.getLeftChild();
        rightChild = tree.getRightChild();

        if(this.isEmpty())
        {
            this.setHeight(0);
        }
        else
        {
            this.height();
        }
    }

    public T getData()
    {
        if(! isEmpty())
        {
            return root.getData();
        }
        else
        {
            return null;
        }
    }

    public Node<T> getRoot()
    {
        return root;
    }

    public int getHeight()
    {
        return height;
    }

    public AVLTree<T> getLeftChild()
    {
        return leftChild;
    }

    public AVLTree<T> getRightChild()
    {
        return rightChild;
    }

    public void setHeight(int n)
    {
        height = n;
    }

}
