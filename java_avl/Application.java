public class Application
{
    public static void main(String[] arg)
    {
        AVLTree<Double> abr = new AVLTree<Double>();

        for( double i = 0; i < 400; i++)
        {
            //if( i%2 == 0)
            //{
                //abr.add(i);
            //}
            //else
            //{
                //abr.add(-i);
            //}

            abr.add(i);
        }

        //for( double j = 0; j < 399; j++)
        //{
            //abr.delete(j);
            //System.out.println(abr.findMin());
        //}

        abr.display();
        abr.displayStruct();

        System.out.println();
        System.out.println("min = " + abr.findMin());
        System.out.println("max = " + abr.findMax());
        double k = 50;
        //for(double k = 0; k < 40; k = k + 0.5)
        //{
            System.out.println("voisin gauche de " + k + " est : " + abr.leftNeighbor(k));
            System.out.println("voisin droit de " + k + " est : " + abr.rightNeighbor(k));
            //System.out.println();
        //}




    }
}
