class Node:
    def __init__(self, value, prev, next):
        self.value = value
        self.prev = prev
        self.next = next


def insert(value, node):
    left = node.next
    right = left.next
    new_node = Node(value, left, right)
    left.next = new_node
    right.prev = new_node
    return new_node


def remove(node):
    for _ in range(7):
        node = node.prev
    node.prev.next = node.next
    node.next.prev = node.next
    return node


NPLAYERS = 416
NROUNDS = 71975 * 100

root = Node(0, None, Node)
root.prev = root
root.next = root

player = 0
current_node = root
scores = {}

for round in range(1, NROUNDS + 1):
    if round % 23 == 0:
        removed_node = remove(current_node)
        current_node = removed_node.next
        scores[player] = scores.get(player, 0) + removed_node.value + round
    else:
        current_node = insert(round, current_node)
    player = (player + 1) % NPLAYERS

best = reversed(sorted(scores.items(), key=lambda x: x[1]))
print(list(best)[:3])
