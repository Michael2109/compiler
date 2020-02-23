package cobalt;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Snail {

    public static int[] snail(final int[][] array) {
        return unwrapRing(array, 0, array.length).stream().mapToInt(Integer::intValue).toArray();
    }

    private static List<Integer> unwrapRing(final int[][] array, final int currentRing, final int ringSize) {

        final int[] result = new int[ringSize == 1 ? 0 : (ringSize - 1) * 4];

        for (int i = 0; i < ringSize - 1; i++) {

            result[i] = array[currentRing][currentRing + i];
            result[i + ringSize - 1] = array[currentRing + i][currentRing + ringSize - 1];
            result[i + (ringSize - 1) * 2] = array[currentRing + ringSize - 1][currentRing + ringSize - 1 - i];
            result[i + (ringSize - 1) * 3] = array[currentRing + ringSize - 1 - i][currentRing];
        }

        final List<Integer> resultList = IntStream.of(result).boxed().collect(Collectors.toList());

        if (ringSize == 1) {
            resultList.add(array[currentRing][currentRing]);
        } else if (ringSize > 2) {
            resultList.addAll(unwrapRing(array, currentRing + 1, ringSize - 2));
        }

        return resultList;
    }
}
