public class FactorialV2 {
  public static void main(String[] args){
	int n = 6;
	int i = 1;
	int fact = 1;
	int sum = 0;
	while (i<=n) {
		fact = fact * i;
		sum = sum + i;
		i += 1;
	}
  }
} 
