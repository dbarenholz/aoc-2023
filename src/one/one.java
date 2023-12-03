package one;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
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

  // note: the string 'oneight' should become '18' as number.
  List<String> preprocess(List<String> input) {
    List<Pair<String, String>> replacements = Arrays.asList(
        new Pair<String, String>("one", "1"),
        new Pair<String, String>("two", "2"),
        new Pair<String, String>("three", "3"),
        new Pair<String, String>("four", "4"),
        new Pair<String, String>("five", "5"),
        new Pair<String, String>("six", "6"),
        new Pair<String, String>("seven", "7"),
        new Pair<String, String>("eight", "8"),
        new Pair<String, String>("nine", "9"));

    return input.stream()
        .map(s -> {
          String res = "";

          for (int i = 0; i < s.length(); i++) {
            for (Pair<String, String> repl : replacements) {

              String check = "[OUT_OF_BOUNDS :(]";
              try {
                check = s.substring(i, i + repl.a.length());
              } catch (StringIndexOutOfBoundsException e) {
              }

              // We found a match here; add replacement
              // Note that we do not care about incrementing 'i';
              // by not incrementing 'i', we correctly parse 'oneight' as 18.
              if (check.equals(repl.a)) {
                res += repl.b;
                break;
              }
            }
            // No match; add character and continue -- again ignore out of bounds exceptions
            try {
              res += s.charAt(i);
            } catch (StringIndexOutOfBoundsException e) {
            }
          }
          return res;
        })
        .toList();
  }

  int computeSum(List<String> input) {
    return input.stream()
        .map(s -> s.replaceAll("[a-z]*", "")) // ignore all letters
        .map(s -> s.length() == 1 ? s + "" + s : s) // if only 1 character, double it
        .map(s -> s.charAt(0) + "" + s.charAt(s.length() - 1)) // grab first and last character
        .map(s -> Integer.parseInt(s)) // pretend it's an integer
        .reduce(0, (a, b) -> a + b); // and sum everything
  }

  private void run() throws IOException {
    List<String> lines = preprocess(input()); // preprocessing: part 2
    int result = computeSum(lines);

    BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));
    w.write(result + "\n");
    w.flush();
  }

  public static void main(String[] a) throws IOException {
    (new one()).run();
  }

}

class Pair<A, B> {
  A a;
  B b;

  public Pair(A a, B b) {
    this.a = a;
    this.b = b;
  }
}