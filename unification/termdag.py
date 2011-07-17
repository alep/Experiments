# -*- coding: utf-8 -*-
import sys
from pprint import pprint


class TermNode(object):

    def __init__(self, symbol):
        self.symbol  = symbol
        self._parents = []
        self._children = []

    def _add_parent(self, node):
        """
        This function appends a parent to the node,
        parents should only be appended when we append self
        to another node.
        """
        self._parents.append(node)

    def empty_parents(self):
        """
        This function removes all parents
        """
        self._parents = []

    @property
    def parents(self):
        return self._parents

    @property
    def children(self):
        return self._children

    def _append_term(self, node):
        # check that it's a node, or at least has its interface
        if not hasattr(node, '_add_parent'):
            typename = str(type(node))
            raise TypeError('Node expected, instead ' + typename + ' given')

        self._children.append(node)
        node._add_parent(self)

    def __len__(self):
        return len(self._children)
    
    def __getitem__(self, index):
        if len(self) <= index:
            raise IndexError('index out of range')
        else:
            return self._children[index]

    def __setitem__(self, index, value):
        if len(self) <= index:
            raise IndexError('assignment index out of range')
        else:
            # value should be a node
            if not hasattr(value, '_add_parent'):
                typename = str(type(node))
                raise TypeError('Node expected, instead ' + typename + ' given')

            self._children[index] = value
            value._add_parent(self)

    def __contains__(self, other):
        stack = [self]

        while stack:
            elem = stack.pop(0)
            if other is elem:
                return True
            else:
                stack += elem.children
                
        return False

    def __repr__(self):
        return 'TermNode(' + repr(self.symbol) + ')'

    def is_function_node(self):
        return False

    def is_variable_node(self):
        return False

    def __eq__(self, other):
        return self.symbol == other.symbol

class VariableNode(TermNode):

    def is_variable_node(self):
        return True

    def __repr__(self):
        return "VariableNode(" + repr(self.symbol) + ")"


class FunctionNode(TermNode):

    def __init__(self, symbol, outdegree, children_list):
        if len(children_list) != outdegree:
            raise ValueError('The list of children is different'
                             ' than the outdegree')

        super(FunctionNode, self).__init__(symbol)
        self.outdegree = outdegree
        
        for child in children_list:
            self._append_term(child)

    def is_function_node(self):
        return True

    def __eq__(self, other):
        return (self.symbol == other.symbol and
                self.outdegree == other.outdegree)

    def __repr__(self):
        return ('FuntionNode(' + 
                repr(self.symbol) + ', '  +
                repr(self.outdegree) + ', ' +
                repr(self.children) + ')')


class ConstantNode(FunctionNode):

    def __init__(self, symbol):
        super(ConstantNode, self).__init__(symbol, 0, [])

    def __repr__(self):
        return ('ConstantNode(' +
                repr(self.symbol))
        
class SymbolClashException(Exception):
    pass

class OccursCheckException(Exception):
    pass

class TermDAG(object):
    """
    Represents a directe acyclic graph for terms
    """
    def replace(self, node, othernode):
        """
        Merges two node, say we have nodes s and t and we wan to merge them

        Let parents(s) = {p1, ..., pn }; then
        1. For each pi, replace the subterm arc pi -> s by pi -> t
            Note: this is done by p[k] = othernode in the code, where k is the
            position of the node 's' in the list and othernode is 't'
        2. Let parents(t) := parents(s) U parents(t);
            Note: U represents the set union; in code this is done
            when we you use __setitem__, that is p[k] = othernode
        3. Let parents(s) := Empty
        """

        for p in node.parents:
            for k, child in enumerate(p.children):
                if node is child:
                    p[k] = othernode
        node.empty_parents()

    def unify(self, node, other, sigma=None):
        if sigma is None:
            sigma = []
            
        if node is other:
            return

        elif node.is_function_node() and other.is_function_node():
            if node == other:
                for i in xrange(0, node.outdegree):
                    self.unify(node[i], other[i], sigma)
            else:
                raise SymbolClashException()
           
        elif not node.is_variable_node():
            self.unify(other, node, sigma)

        elif node in other:
            raise OccursCheckException()

        else:
            sigma.append((node, other))
            self.replace(node, other)
            
        return sigma

if __name__ == '__main__':
    y = VariableNode('y')

    f = FunctionNode('f', 2, [FunctionNode('g', 1, [y]),
                              FunctionNode('g', 1, [y]),
                              ])

    f_ = FunctionNode('f', 2, [VariableNode('x'),
                               FunctionNode('g', 1, [ConstantNode('a')])])

    print repr(f)
    print '--'

    print repr(f_)
    print '--'
    
    dag_manager = TermDAG()


    try:
        sigma = dag_manager.unify(f, f_)
    except SymbolClashException:
        print 'Failed to unify'
    except OccursCheckException:
        print 'Failed to unify'
    else:
        print sigma
    
    
