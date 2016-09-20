public class ConstantV2{
    public static void main(String[] argv){
        int z = 3;
        int x = 0;
	int y = 1;
	while (y > 0) {
		if(y==1) {
			x = 7;
		}
		else {
			x = z + 4;
		}
		y = 3;
	}
    }
}
