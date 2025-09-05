#!/usr/bin/python

# Generates a random MiniML program of a specified width (number of topexpressions) and height (levels of expressions per topexpression).
# The generated program only contains monomorphic types.

import random
import argparse

INT_COUNT = 0
FUN_COUNT = 0


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--width", dest="width", type=int)
    parser.add_argument("--height", dest="height", type=int)
    parser.add_argument("-o", dest="out", type=str)
    args = parser.parse_args()

    width = args.width
    height = args.height
    path = args.out

    if width is None or height is None or path is None:
        print(
            "Usage: python miniml_generator.py --width <width> --height <height> -o <output path>"
        )
        return

    if width < 1 or height < 1:
        print("Width and Height must be non-negative integers.")
        return

    with open(path, "w") as f:
        f.write(gen_topexpr(width, height, [], []))


def gen_topexpr(w: int, h: int, ints: list, funs: list) -> str:
    program = ""
    generated_expr_count = 1
    while generated_expr_count < w:
        gen_choices = ["int", "fun"]
        choice = random.choice(gen_choices)
        if choice == "int":
            x = new_int_id()
            program += f"let {x} = {gen_expr(h, ints, funs)};;\n"
            ints.append(x)
        elif choice == "fun":
            f = new_fun_id()
            g = new_fun_id()
            x = new_int_id()
            program += f"let {f} = fun {g} ({x}:int) is {x} + {gen_expr(h, ints + [x], funs + [g])};;\n"
            funs.append(f)

        generated_expr_count += 1

    program += f"{gen_expr(h, ints, funs)};;"
    return program


def gen_expr(h: int, ints: list, funs: list) -> str:
    if h == 1:
        if len(ints) == 0:
            return random_int()

        choice = random.randint(0, 1)
        if choice == 0:
            return random_int()
        else:
            return random.choice(ints)

    gen_choices = ["if", "add", "sub", "mul", "inline call", "call"]
    if len(funs) == 0:
        choice = random.choice(gen_choices[:4])
    else:
        choice = random.choice(gen_choices)

    match choice:
        case "if":
            return f"if (({gen_expr(h - 1, ints, funs)}) {gen_comp()} ({gen_expr(h - 1, ints, funs)})) then ({gen_expr(h - 1, ints, funs)}) else ({gen_expr(h - 1, ints, funs)})"
        case "add":
            return f"{gen_expr(h - 1, ints, funs)} + {gen_expr(h - 1, ints, funs)}"
        case "sub":
            return f"{gen_expr(h - 1, ints, funs)} - {gen_expr(h - 1, ints, funs)}"
        case "mul":
            return f"{gen_expr(h - 1, ints, funs)} * {gen_expr(h - 1, ints, funs)}"
        case "inline call":
            f = new_fun_id()
            x = new_int_id()
            return f"((fun {f} ({x}:int) is {x} + {gen_expr(h - 1, ints + [x], funs + [f])})({gen_expr(h - 1, ints, funs)}))"
        case "call":
            return f"{random.choice(funs)}({gen_expr(h - 1, ints, funs)})"


def random_int():
    return random.randint(0, 100)


def gen_comp():
    comps = ["<", "="]
    return random.choice(comps)


def new_int_id():
    global INT_COUNT
    new_id = f"t_{INT_COUNT}"
    INT_COUNT += 1
    return new_id


def new_fun_id():
    global FUN_COUNT
    new_id = f"f_{FUN_COUNT}"
    FUN_COUNT += 1
    return new_id


def reset_counts():
    global INT_COUNT, FUN_COUNT
    INT_COUNT = 0
    FUN_COUNT = 0


if __name__ == "__main__":
    main()
