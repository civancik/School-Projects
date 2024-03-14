import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.CountDownLatch;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int CAPACITY = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;
    private CountDownLatch doneSignal = new CountDownLatch(TOTAL_CUSTOMERS);

    public Semaphore saleUpdate = new Semaphore(1); // 1 Cashier can update sales at a time
    public Semaphore registers = new Semaphore(4); // 4 registers
    // Semaphore for each shelf
    public Semaphore breadShelves = new Semaphore(3); // 3 bread shelves
     /*Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        executor = Executors.newFixedThreadPool(CAPACITY);

        for(int i = 0; i < TOTAL_CUSTOMERS; i++) {
            executor.execute(new Customer(this, doneSignal));
            i++;
        }
        executor.shutdown();
    
        try {
            doneSignal.await();
            System.out.printf("Total Sales = %.2f\n", sales);
            executor.shutdown();
        } catch(InterruptedException ie) {
            ie.printStackTrace();
        }
    }
}
