import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.Scanner;
import java.util.List;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

class Instruction {
    public int operand;
    public String opcode;
    Instruction(String opcode, int operand) {
        this.opcode = opcode;
        this.operand = operand;
    }

    @Override
    public String toString() {
        return opcode + " " + (operand > 0 ? "+" : "") + Integer.toString(operand);
    }
}

class State {
    public int ip;
    public int acc;
    State(int ip, int acc) {
        this.ip = ip;
        this.acc = acc;
    }
}

public class Day08 {
    public static void main(String args[]) {
        Pattern pattern = Pattern.compile("(nop|acc|jmp) ([-+][0-9]+)");
        Scanner scanner = new Scanner(System.in);
        List<Instruction> instructions = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            Matcher matcher = pattern.matcher(line);
            if (!matcher.find()) {
                return;
            }
            String opcode = matcher.group(1);
            int operand = Integer.parseInt(matcher.group(2));
            instructions.add(new Instruction(opcode, operand));
        }
        scanner.close();
        State part1State = compute(instructions);
        System.out.println(part1State.acc);
        State part2State = null;
        for (int index = 0; index < instructions.size(); index++) {
            List<Instruction> copy = new ArrayList<>(instructions);
            Instruction target = copy.get(index);
            if (target.opcode.equals("acc")) {
                continue;
            }
            Instruction changed = new Instruction(target.opcode.equals("jmp") ? "nop" : "jmp", target.operand);
            copy.set(index, changed);
            part2State = compute(copy);
            if (part2State.ip >= copy.size()) {
                break;
            }
        }
        System.out.println(part2State.acc);
    }

    public static State compute(List<Instruction> instructions) {
        State state = new State(0, 0);
        Set<Integer> seen = new HashSet<>();
        while (state.ip < instructions.size()) {
            if (seen.contains(state.ip)) {
                break;
            }
            seen.add(state.ip);
            Instruction instruction = instructions.get(state.ip);
            switch (instruction.opcode) {
            case "jmp":
                state.ip = instruction.operand + state.ip;
                break;
            case "acc":
                state.acc += instruction.operand;
                state.ip++;
                break;
            case "nop":
                state.ip++;
                break;
            }

        }
        return state;
    }
}

// Local Variables:
// compile-command: "javac Day08.java && java Day08 < input.txt"
// End:
