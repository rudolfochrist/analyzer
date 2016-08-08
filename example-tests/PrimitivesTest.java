import org.junit.Test;

public class PrimitivesTest {

    @Test
    public void test() {
        byte b = 1;
        short s = 1;
        int i = 1;
        long l = 1;
        float f = 1.0f;
        double d = 1.0;
        char c = '1';
        boolean bl = true;

        b += test();
        s += test();
        i += test();
        l += test();
        f += test();
        d += test();
        c += test();
        bl = test();
    }
}
