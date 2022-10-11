/**
 * cse250.pa5.AVLTreeMap.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: arflierl
 * Person#: 31245340
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:bmb4
 */
package cse250.pa4

import cse250.examples.types.mutable.Map

import collection.mutable.Stack

class AVLTreeMap[K, V]()(implicit ord: Ordering[K]) extends Map[K, V]{
  val _storageTree = new AVLTree[K, V]

  override def addOne(elem: (K, V)): Unit = _storageTree.insert(elem)

  override def removeOne(key: K): Boolean = _storageTree.remove(key)

  override def get(key: K): Option[V] = _storageTree.find(key) match {
    case n: _storageTree.AVLNode[(K, V)] if n != null => Some(n._value._2)
    case null                                         => None
  }

  override def iterator: Iterator[(K, V)] = _storageTree.iterator
}

class AVLTree[K, V]()(implicit ord: Ordering[K]) {

  class AVLNode[A](var _value: A, var _left: AVLNode[A], var _right: AVLNode[A], var _parent: AVLNode[A],
                   var _leftH: Boolean, var _rightH: Boolean)

  var _avlRoot: AVLNode[(K, V)] = null

  def find(elem: K): AVLNode[(K, V)] = {
    var current = _avlRoot
    var found = false
    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(elem, currentKey)) current = current._left
      else if (ord.lt(currentKey, elem)) current = current._right
      else found = true
    }
    current
  }
  //lecture notes were used to come up with format for rotation methods
  def rotateLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    if (nodeA == _avlRoot) _avlRoot = nodeA._right
    else nodeA._right._parent = nodeA._parent
    if (nodeA._parent != null && nodeA == nodeA._parent._right) {
      nodeA._parent._right = nodeA._right
      nodeA._parent = nodeA._right  //node B is the new parent of node A
    }else if (nodeA._parent != null && nodeA == nodeA._parent._left) {
      nodeA._parent._left = nodeA._right
      nodeA._parent = nodeA._right
    }else {
      nodeA._parent = nodeA._right
      nodeA._right._parent = null
    }
    if (nodeA._right._left != null) {
      nodeA._right = nodeA._right._left
      nodeA._right._parent = nodeA
    }
    else nodeA._right = null //left subtree of B becomes right subtree of A
    nodeA._parent._left = nodeA
    nodeA._parent
  }

  def rotateRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    if (nodeA == _avlRoot) {
      _avlRoot = nodeA._left
    }
    else nodeA._left._parent = nodeA._parent
    if (nodeA._parent != null && nodeA == nodeA._parent._left) {
      nodeA._parent._left = nodeA._left
      nodeA._parent = nodeA._left
    }else if (nodeA._parent != null && nodeA == nodeA._parent._right) {
      nodeA._parent._right = nodeA._left
      nodeA._parent = nodeA._left
    }else {
      nodeA._parent = nodeA._left
      nodeA._left._parent = null
    }
    if (nodeA._left._right != null) {
      nodeA._left = nodeA._left._right
      nodeA._left._parent = nodeA
    }
    else nodeA._left = null
    nodeA._parent._right = nodeA
    nodeA._parent
  }

  def rotateLeftRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    rotateLeft(nodeA._left)
    rotateRight(nodeA)
  }

  def rotateRightLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    rotateRight(nodeA._right)
    rotateLeft(nodeA)
  }

  def insert(elem: (K, V)): AVLNode[(K, V)] = {
    val node = new AVLNode[(K, V)](_value = elem, _left = null, _right = null, _parent = null, _leftH = false, _rightH = false)
    if (_avlRoot == null) {
      _avlRoot = node
      return node
    }
    //now I know that the root of the tree is not null
    //I got the idea for add from the course textbook:
    //Object-Orientation, Abstraction, and Data Structures Using Scala
    //
    //Mark C. Lewis, Trinity University
    @scala.annotation.tailrec
    def add(rover: AVLNode[(K, V)]): Unit = {
      if (ord.lt(elem._1, rover._value._1)) {
        if (rover._left == null) {
          rover._left = node
          node._parent = rover
        } else add(rover._left)
      }else if (ord.lt(rover._value._1, elem._1)) {
        if (rover._right == null) {
          rover._right = node
          node._parent = rover
        }else add(rover._right)
      }else {
        rover._value = elem
      }
    }
    //my own clever device lol
    def tumbler(Node: AVLNode[(K, V)]): Unit = {
      var balanced = false
      var cur = Node
      while (cur != null && !balanced) {
        if (cur._parent != null && cur == cur._parent._left) {
          if (!cur._parent._leftH) {
            if (cur._parent._rightH) {
              cur._parent._rightH = false
              balanced = true
            } else cur._parent._leftH = true
          }
          else {
            //BF -2
            if (cur._leftH) {
              rotateRight(cur._parent)
              cur._right._leftH = false
              cur._leftH = false
            }
            if (cur._rightH) {
              rotateLeftRight(cur._parent)
              if (cur._parent._leftH) {
                cur._parent._right._rightH = true
                cur._parent._right._leftH = false
                cur._rightH = false
                cur._leftH = false
                cur._parent._leftH = false
                cur._parent._rightH = false
              }else if (cur._parent._rightH) {
                cur._parent._right._rightH = false
                cur._parent._right._leftH = false
                cur._parent._rightH = false
                cur._parent._leftH = false
                cur._rightH = false
                cur._leftH = true
              }else {
                cur._rightH = false
                cur._leftH = false
                cur._parent._rightH = false
                cur._parent._leftH = false
                cur._parent._right._rightH = false
                cur._parent._right._leftH = false
              }
            }
            balanced = true
          }
        }
        if (cur._parent != null && cur == cur._parent._right) {
          if (!cur._parent._rightH) {
            if (cur._parent._leftH) {
              cur._parent._leftH = false
              balanced = true
            }else cur._parent._rightH = true
          }
          else {
            //BF +2
            //current parent IS right heavy
            //after the rotation, cur._parent references a different node so watch out!
            if (cur._rightH) {
              rotateLeft(cur._parent)
              cur._left._rightH = false
              cur._rightH = false
            }
            if (cur._leftH) {
              rotateRightLeft(cur._parent)
              if (cur._parent._rightH) {
                cur._parent._left._rightH = false
                cur._parent._left._leftH = true
                cur._leftH = false
                cur._rightH = false
                cur._parent._rightH = false
                cur._parent._leftH = false
              }else if (cur._parent._leftH) {
                cur._leftH = false
                cur._rightH = true
                cur._parent._left._rightH = false
                cur._parent._left._leftH = false
                cur._parent._leftH = false
                cur._parent._rightH = false
              } else {
                cur._parent._left._rightH = false
                cur._parent._left._leftH = false
                cur._leftH = false
                cur._rightH = false
                cur._parent._leftH= false
                cur._parent._rightH = false
              }
            }
            balanced = true
          }
        }
        if (cur._parent != null) cur = cur._parent
        else cur = null
      }
    }
    add(_avlRoot)
    tumbler(node)
    node
  }

  def remove(key: K): Boolean = {
    var removeLeft = false
    var removeRight = false
    var removed = false
    var depthOfSubtreeUnchanged = false
    val victim = find(key)
    if (victim == null) return false
    @scala.annotation.tailrec
    def goodCop_badCop(BenMatlock: AVLNode[(K, V)]): AVLNode[(K, V)] = {
      if (BenMatlock._left == null) BenMatlock
      else goodCop_badCop(BenMatlock._left)
    }
    //this is again based on the findVictim method from the course textbook
    //Object-Orientation, Abstraction, and Data Structures Using Scala
    //
    //Mark C. Lewis, Trinity University
    def murderSheWrote(BenMatlocksClient: AVLNode[(K, V)]): AVLNode[(K, V)] = {
      removeLeft = false
      removeRight = false
      if (BenMatlocksClient._left == null && BenMatlocksClient._right == null) {
        //we are removing a leaf
        //make sure it isn't the root
        //if it is, we will return the root and check for that afterwards : )
        //and if it is not the root, then it must have a parent
        if (BenMatlocksClient == _avlRoot) return BenMatlocksClient
        if (BenMatlocksClient._parent._left == BenMatlocksClient) {
          if (!BenMatlocksClient._parent._rightH && !BenMatlocksClient._parent._leftH) depthOfSubtreeUnchanged = true
          removeLeft = true
          BenMatlocksClient._parent
        } else {
          if (!BenMatlocksClient._parent._rightH && !BenMatlocksClient._parent._leftH) depthOfSubtreeUnchanged = true
          removeRight = true
          BenMatlocksClient._parent
        }
      } else if (BenMatlocksClient._right == null && BenMatlocksClient._left != null) {
        //we are removing a node with ONE LEFT CHILD
        BenMatlocksClient._value = BenMatlocksClient._left._value
        removeLeft = true
        BenMatlocksClient
      } else if (BenMatlocksClient._left == null && BenMatlocksClient._right != null) {
        //we are removing a node with ONE RIGHT CHILD
        BenMatlocksClient._value = BenMatlocksClient._right._value
        removeRight = true
        BenMatlocksClient
      } else {
        //we are removing a node with TWO CHILDREN
        //pass BenMatlockClient.right to
        val saulGoodman = goodCop_badCop(BenMatlocksClient._right)
        BenMatlocksClient._value = saulGoodman._value
        removeLeft = true
        saulGoodman._parent
      }
    }
    //my own creation
    @scala.annotation.tailrec
    def judgeAndJury(executioner: AVLNode[(K, V)]): Unit = {
      if (removeLeft) {
        if (!executioner._leftH && !executioner._rightH) {
          executioner._rightH = true
          if (!removed) executioner._left = null
          removed = true
          if (executioner._parent != null && !depthOfSubtreeUnchanged) judgeAndJury(executioner._parent)
        } else if (executioner._leftH && !executioner._rightH) {
          executioner._leftH = false
          if (!removed) executioner._left = null
          removed = true
          if (executioner._parent != null) judgeAndJury(executioner._parent)
        } else {
          if (executioner._right != null && executioner._right._rightH) {
            if (!removed) executioner._left = null
            removed = true
            rotateLeft(executioner)
            executioner._rightH = false
            executioner._parent._rightH = false
            if (executioner._parent != null) judgeAndJury(executioner._parent)
          } else if (executioner._right != null && executioner._right._leftH) {
            if (!removed) executioner._left = null
            removed = true
            rotateRightLeft(executioner)
            if (executioner._parent != null && executioner._parent._rightH) {
              executioner._rightH = false //executioner
              executioner._leftH = true //executioner
              executioner._parent._right._leftH = false //executioner.parent.right
              executioner._parent._rightH = false //executioner.parent
              if (executioner._parent != null) judgeAndJury(executioner._parent)
            } else if (executioner._parent != null && executioner._parent._leftH) {
              executioner._parent._right._leftH = false //executioner.parent.right
              executioner._parent._right._rightH = true //executioner.parent.right
              executioner._rightH = false //executioner
              executioner._leftH = false //executioner
              executioner._parent._leftH = false //executioner.parent
              if (executioner._parent != null) judgeAndJury(executioner._parent)
            } else {
              executioner._rightH = false //executioner
              executioner._leftH = false //executioner
              if (executioner._parent._right != null) executioner._parent._right._leftH = false //executioner.parent.right
              if (executioner._parent != null) judgeAndJury(executioner._parent)
            }
          } else {
            if (executioner._rightH) executioner._rightH = false
            if (executioner._parent != null) judgeAndJury(executioner._parent)
          }
        }
      } else if (removeRight) {
        if (!executioner._leftH && !executioner._rightH) {
          executioner._leftH = true
          if (!removed) executioner._right = null
          removed = true
          if (executioner._parent != null && !depthOfSubtreeUnchanged) judgeAndJury(executioner._parent)
        } else if (!executioner._leftH && executioner._rightH) {
          executioner._rightH = false
          executioner._right = null
          removed = true
          if (executioner._parent != null) judgeAndJury(executioner._parent)
        } else {
          if (executioner._left != null && executioner._left._leftH) {
            if (!removed) executioner._right = null
            removed = true
            rotateRight(executioner)
            executioner._leftH = false
            executioner._parent._leftH = false
            if (executioner._parent != null) judgeAndJury(executioner._parent)
          } else if (executioner._left != null && executioner._left._rightH) {
            rotateLeftRight(executioner)
            if (executioner._parent._leftH) {
              executioner._leftH = false
              executioner._rightH = true
              executioner._parent._left._rightH = false
              executioner._parent._leftH = false
              if (executioner._parent != null) judgeAndJury(executioner._parent)
            } else if (executioner._parent._rightH) {
              executioner._parent._left._rightH = false
              executioner._parent._left._leftH = true
              executioner._leftH = false
              executioner._rightH = false
              executioner._parent._rightH = false
              if (executioner._parent != null) judgeAndJury(executioner._parent)
            } else {
              executioner._rightH = false
              executioner._leftH = false
              if (executioner._parent != null && executioner._parent._left != null) executioner._parent._left._rightH = false
              if (executioner._parent != null) judgeAndJury(executioner._parent)
            }
          }
        }
      } else {
        if (executioner._leftH) executioner._leftH = false
        if (executioner._parent != null) judgeAndJury(executioner._parent)
      }
    }
    //you will notice that the 'victim' gets passed to murderSheWrote
    //where 'victim' becomes BenMatlocksClient
    //after that Saul Goodman is sent to the judgeAndJury
    //and if convicted he will encounter the executioner who then restores order
    judgeAndJury(murderSheWrote(victim))
    true
  }

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val _parentStack = {
      val stack = new Stack[AVLNode[(K, V)]]
      var currentNode = _avlRoot
      while (currentNode != null) {
        stack.push(currentNode)
        currentNode = currentNode._left
      }
      stack
    }

    override def hasNext: Boolean = _parentStack.nonEmpty

    override def next(): (K, V) = {
      val originalTop = _parentStack.top
      if (originalTop._right != null) {
        var currentNode = originalTop._right
        while (currentNode != null) {
          _parentStack.push(currentNode)
          currentNode = currentNode._left
        }
      }
      else {
        var recentTop = _parentStack.pop
        while (_parentStack.nonEmpty && recentTop != _parentStack.top._left) {
          recentTop = _parentStack.pop
        }
      }
      originalTop._value
    }
  }
}
