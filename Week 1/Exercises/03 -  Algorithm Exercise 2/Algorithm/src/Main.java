public class Main {
    public static void main(String[] args) {

        int[] numbers = { 3, 7, 2, 1, 5, 12, 4, 0 };

        // 1 Put in the chamber 1 the number in the chamber 8
        numbers[0] = numbers[7];

        int temp = 1;

        do{

            //2 - Sum the number in the chamber 1 to the number in the chamber 2 and put the total
            //in the chamber 1
            numbers[0] += numbers[temp];

            //3 - Change the instruction 2 adding in 1 the second number of the chamber that is mentioned in that instruction
            temp++;

            //4 - Is the second number in the chamber in the instruction 2 mentioned before than the number in the chamber 7?
            System.out.println("Number in the chamber 1: " + numbers[0]);

        } while (temp < numbers[6]);

    }

}