public class SortV1 {
  public static void main(String[] args){
	int[] data = {10,20,30,40,50,60,71,80,90,91};
	int size = 10;
	for (int i = 0; i < size - 1; i++) {
            int minPos = i;
            for (int j = i + 1; j < size; j++) {
                if (data[j] < data[minPos]) {
                    minPos = j;
                }
            }
        }
  }
} 
