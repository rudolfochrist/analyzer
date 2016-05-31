import org.junit.Assert;
import org.junit.Test;

public class MissingSemicolonTest {

    @Test
    public void foo() throws Exception {
        Foo f = new Foo()
        Assert.assertEquals("foo", f.getName());
    }
}
