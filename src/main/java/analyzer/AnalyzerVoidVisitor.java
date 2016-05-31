package analyzer;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.visitor.VoidVisitor;

public interface AnalyzerVoidVisitor<A> extends VoidVisitor<A> {

	public void visit(Node node, A arg);
}
