import java.sql.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;
    private CountDownLatch doneSignal;
    
    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery, CountDownLatch l) {
        this.bakery = bakery;
        this.rnd = new Random();
        this.shopTime = rnd.nextInt(200);
        this.checkoutTime = rnd.nextInt(200);
        this.shoppingCart = new ArrayList<BreadType>();
        this.doneSignal = l;
    }

    /**
     * Run tasks for the customer
     */
    public void run() { 

        try {
            // 1. Fill Shopping Cart
            this.fillShoppingCart();

            // 2. Take bread from shelves and update shelves
            this.bakery.breadShelves.acquire();
            for (BreadType bread : shoppingCart) {
                this.bakery.takeBread(bread);
            }
            this.bakery.breadShelves.release();

            // 3. Wait shop time
            //System.out.println(this + " is shopping for " + shopTime + "ms\n");
            Thread.sleep(shopTime);

            // 4. Go to a register
            this.bakery.registers.acquire();

            // 5. Update Salse
            this.bakery.saleUpdate.acquire();
            this.bakery.addSales(getItemsValue());
            this.bakery.saleUpdate.release();

            // 6. Wait checkout time
            //System.out.println(this + " is checkingout for " + checkoutTime + "ms\n");
            Thread.sleep(checkoutTime);
            this.bakery.registers.release();

            // 7. Fin
            doneSignal.countDown();

        } catch (Exception e) {
        e.printStackTrace();
    }
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}
