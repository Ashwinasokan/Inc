public class Search {
  public static void main(String[] args){
    boolean found = false;
    int[] data = {10,20,30,40,50,60,71,80,90,91};
    int size = 10;
    int key = 50;
    int low = 0;
    int high = size - 1;  
    while(high >= low) {
	int middle = (low + high) / 2;
     	if(data[middle] == key) {
		found = true;
	}
	if(data[middle] < key) {
		low = middle + 1;
	}
	if(data[middle] > key) {
		high = middle - 1;
	}
    }
 }
}
