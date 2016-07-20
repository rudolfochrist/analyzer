
public class ThisTest {

    private BoardA boardA = new BoardA();
    private BoardB boardB = new BoardB();
    private BoardC boardC = new BoardC();
    private BoardD boardD = new BoardD();

    @Test
    public void test1() {
        Value ba = this.boardA.stuff();
        Value bb = boardB.stuff();
        Number bc = this.boardC.getWidth().getInt();
        Number bd = boardD.getWidth().getInt();
        Value be = BoardE.anyVal();
    }
}
