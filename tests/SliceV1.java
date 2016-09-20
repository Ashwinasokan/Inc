public class SliceV1 {
	public static void main(String[] args) {
		int n = 10;
		int sum = 0;
		int product = 1;
		int i = 1;
		while (i <= n) {
			sum += i;
			product *= i;
			i += 1;
		}
	}
}
