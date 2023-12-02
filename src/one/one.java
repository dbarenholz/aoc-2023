package one;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

class one {

  List<String> input() throws IOException {
    ArrayList<String> lines = new ArrayList<>();

    BufferedReader r = new BufferedReader(new FileReader("/home/dan/dev/aoc-2023/src/one/input.txt"));
    String line;
    while ((line = r.readLine()) != null) {
      lines.add(line);
    }
    r.close();

    return lines;
  }

  int computeSum(List<String> input) {
    return input.stream()
        .map(s -> s.replaceAll("[a-z]*", ""))
        .map(s -> s.length() == 1 ? s + "" + s : s)
        .map(s -> s.charAt(0) + "" + s.charAt(s.length() - 1))
        .map(s -> Integer.parseInt(s))
        .reduce(0, (a, b) -> a + b);
  }

  private void run() throws IOException {
    List<String> lines = input();
    int result = computeSum(lines);

    BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));
    w.write(result + "\n");
    w.flush();
  }

  public static void main(String[] a) throws IOException {
    (new one()).run();
  }

}
