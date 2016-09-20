public class ConstantV1{
    public static void main(String[] argv){
        int z = 3;
        int x = 1;
	int y = 0;
	while (x > 0) {
		if(x==1) {
			y = 7;
		}
		else {
			y = z + 4;
		}
		x = 3;
	}
    }
}
