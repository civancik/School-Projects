/*
 * scull.c -- the bare scull char module
 *
 * Copyright (C) 2001 Alessandro Rubini and Jonathan Corbet
 * Copyright (C) 2001 O'Reilly & Associates
 *
 * The source code in this file can be freely used, adapted,
 * and redistributed in source or binary form, so long as an
 * acknowledgment appears in derived source files.  The citation
 * should list that the code comes from the book "Linux Device
 * Drivers" by Alessandro Rubini and Jonathan Corbet, published
 * by O'Reilly & Associates.   No warranty is attached;
 * we cannot take responsibility for errors or fitness for use.
 *
 */

#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/init.h>

#include <linux/kernel.h>	/* printk() */
#include <linux/slab.h>		/* kmalloc() */
#include <linux/fs.h>		/* everything... */
#include <linux/errno.h>	/* error codes */
#include <linux/types.h>	/* size_t */
#include <linux/cdev.h>

#include <linux/uaccess.h>	/* copy_*_user */
#include <linux/semaphore.h>

#include "scull.h"		/* local definitions */
//Define mutex and semaphores for read and write calls
struct semaphore empty, full;
static DEFINE_MUTEX(mutex);

// Create three char pointers, one for the que and two to signify start and end for read/write
char * fifo_que;
char *start;
char *end;


/*
 * Our parameters which can be set at load time.
 */

static int scull_major =   SCULL_MAJOR;
static int scull_minor =   0;
static int scull_fifo_elemsz = SCULL_FIFO_ELEMSZ_DEFAULT; /* ELEMSZ */
static int scull_fifo_size   = SCULL_FIFO_SIZE_DEFAULT;   /* N      */

module_param(scull_major, int, S_IRUGO);
module_param(scull_minor, int, S_IRUGO);
module_param(scull_fifo_size, int, S_IRUGO);
module_param(scull_fifo_elemsz, int, S_IRUGO);

MODULE_AUTHOR("civancik");
MODULE_LICENSE("Dual BSD/GPL");

static struct cdev scull_cdev;		/* Char device structure */

/*
 * Open and close
 */

static int scull_open(struct inode *inode, struct file *filp)
{
	printk(KERN_INFO "scull open\n");
	return 0;          /* success */
}

static int scull_release(struct inode *inode, struct file *filp)
{
	printk(KERN_INFO "scull close\n");
	return 0;
}

/*
 * Read and Write
 */
static ssize_t scull_read(struct file *filp, char __user *buf, size_t count, loff_t *f_pos)
{
	/* TODO: implement this function */
	//Make global variables
	size_t read_return;
	size_t read_len;
	size_t temp0;

	//Set up semaphore and error check
	printk(KERN_INFO "scull read\n");
	if(down_interruptible(&full)) {
		printk(KERN_ERR "Error with starting semaphore in read");
		return -ENOTTY;
	};


	//Set up mutex and error check
	if(mutex_lock_interruptible(&mutex)) {
		printk(KERN_ERR "Error with creating mutex in read");
		up(&full);
		return -ENOTTY;
	};

	//Create temp value to store length from the que
	temp0 = *(size_t *)start;
	//Compare count to temp value
	if (count < temp0) {
		read_len = count;
	} else {
		read_len = temp0;
	}

	//move start up to the beginning of the data
	start = start + sizeof(size_t);

	//copy buffer 
	if((copy_to_user(buf, start, read_len)) != 0) {
		printk(KERN_ERR "Error with copy to user in read");
		mutex_unlock(&mutex);
		up(&full);
		return -ENOTTY;
	}

	//move start to beginning on next part of data
	start = start + scull_fifo_elemsz;
	//Check if after the move, start is at the end of the que, if so send it to the beginning
	if (((start - fifo_que)/(scull_fifo_elemsz+sizeof(size_t))) == scull_fifo_size) {
		start = fifo_que;
	}

	//Set return value and unlock mutex/semaphore
	read_return = read_len;
	mutex_unlock(&mutex);
	up(&empty);

	//return bytes copied
	return read_return;

}


static ssize_t scull_write(struct file *filp, const char __user *buf, size_t count, loff_t *f_pos)
{
	//define global variables
	size_t write_return;
	size_t write_len;
	size_t * temp_ptr;

	/* TODO: implement this function */
	printk(KERN_INFO "scull write\n");
	//Set up semaphore
	if(down_interruptible(&empty)) {
		printk(KERN_ERR "Error with semaphore in write");
		return -ENOTTY;
	};

	//set up mutex
	if(mutex_lock_interruptible(&mutex)) {
		printk(KERN_ERR "Error with mutex in write");
		up(&empty);
		return -ENOTTY;
	};

	//Check count compared to elemsz
	if (count > scull_fifo_elemsz) {
		write_len = scull_fifo_elemsz;
	} else {
		write_len = count;
	}

	//create pointer and store length into the que
	temp_ptr = (size_t *) end;
	*temp_ptr = write_len;

	//move end to the beginning of the data
	end = end + sizeof(size_t);

	//write the buffer to the que
	if((copy_from_user(end, buf, write_len)) != 0) {
		printk(KERN_ERR "Error with copy from user in write %ld", write_len);
		mutex_unlock(&mutex);
		up(&full);
		return -ENOTTY;
	}
	
	//move end to the next data section
	end = end + scull_fifo_elemsz;
	
	//check if end is at the end of the que
	if (((end-fifo_que)/(scull_fifo_elemsz+sizeof(size_t))) == scull_fifo_size) {
		end = fifo_que;
	}
	
	//set return values and unlock mutex/semaphore
	write_return = write_len;
	mutex_unlock(&mutex);
	up(&full);
	return write_return;
}

/*
 * The ioctl() implementation
 */
static long scull_ioctl(struct file *filp, unsigned int cmd, unsigned long arg)
{

	int err = 0;
	int retval = 0;
    
	/*
	 * extract the type and number bitfields, and don't decode
	 * wrong cmds: return ENOTTY (inappropriate ioctl) before access_ok()
	 */
	if (_IOC_TYPE(cmd) != SCULL_IOC_MAGIC) return -ENOTTY;
	if (_IOC_NR(cmd) > SCULL_IOC_MAXNR) return -ENOTTY;

	err = !access_ok((void __user *)arg, _IOC_SIZE(cmd));
	if (err) return -EFAULT;

	switch(cmd) {
	case SCULL_IOCGETELEMSZ:
		return scull_fifo_elemsz;


	default:  /* redundant, as cmd was checked against MAXNR */
		return -ENOTTY;
	}
	return retval;

}

struct file_operations scull_fops = {
	.owner 		= THIS_MODULE,
	.unlocked_ioctl = scull_ioctl,
	.open 		= scull_open,
	.release	= scull_release,
	.read 		= scull_read,
	.write 		= scull_write,
};

/*
 * Finally, the module stuff
 */

/*
 * The cleanup function is used to handle initialization failures as well.
 * Thefore, it must be careful to work correctly even if some of the items
 * have not been initialized
 */
void scull_cleanup_module(void)
{
	dev_t devno = MKDEV(scull_major, scull_minor);

	/* TODO: free FIFO safely here */
	kfree(fifo_que);
	/* Get rid of the char dev entry */
	cdev_del(&scull_cdev);

	/* cleanup_module is never called if registering failed */
	unregister_chrdev_region(devno, 1);
}

int scull_init_module(void)
{
	int result;
	dev_t dev = 0;
	

	/*
	 * Get a range of minor numbers to work with, asking for a dynamic
	 * major unless directed otherwise at load time.
	 */
	if (scull_major) {
		dev = MKDEV(scull_major, scull_minor);
		result = register_chrdev_region(dev, 1, "scull");
	} else {
		result = alloc_chrdev_region(&dev, scull_minor, 1, "scull");
		scull_major = MAJOR(dev);
	}
	if (result < 0) {
		printk(KERN_WARNING "scull: can't get major %d\n", scull_major);
		return result;
	}

	cdev_init(&scull_cdev, &scull_fops);
	scull_cdev.owner = THIS_MODULE;
	result = cdev_add (&scull_cdev, dev, 1);
	/* Fail gracefully if need be */
	if (result) {
		printk(KERN_NOTICE "Error %d adding scull character device", result);
		goto fail;
	}

	/* TODO: allocate FIFO correctly here */

	printk(KERN_INFO "scull: FIFO SIZE=%u, ELEMSZ=%u\n", scull_fifo_size, scull_fifo_elemsz);
	

	//Initiate semaphores
	sema_init(&full, 0);
	sema_init(&empty, scull_fifo_size);

	//create que
	fifo_que = kzalloc(scull_fifo_size*(sizeof(size_t) + scull_fifo_elemsz), GFP_KERNEL);
	
	//error check que
	if (!(fifo_que)) {
		printk(KERN_ERR "Failed to allocate memory for FIFO Que");
		return -ENOTTY;
	}
	
	//create pointers for que
	start = fifo_que;
	end = fifo_que;
	//end is beginning of writing
	//start is beginning of reading

	return 0; /* succeed */

  fail:
	scull_cleanup_module();
	return result;
}

module_init(scull_init_module);
module_exit(scull_cleanup_module);
