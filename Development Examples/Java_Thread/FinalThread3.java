import java.io.*;
import java.nio.file.*;
import java.util.*;

public class FinalThread3 extends Thread{

    private ArrayList<String> buildings;
    private HashMap<String,Integer> finishCosts;
    private HashMap<String,HashMap> allCosts;
    private String startBuilding;

    //Instance Constructor
    //Read in InputFile, OutputFile, and page size from calling runner app.
    public FinalThread3(ArrayList<String> theseBuildings, String startBuilding, HashMap<String,Integer> endCosts, HashMap<String,HashMap> travelCosts){
        this.buildings = theseBuildings;
        this.finishCosts = endCosts;
        this.allCosts = travelCosts;
        this.startBuilding = startBuilding;
    }  //Creator

    public static int getSingleCost(String[] sequence, HashMap<String,HashMap> transitCosts){
        int totalCost = 0;
        for(int pos = 0;pos<sequence.length-1;pos++){
            HashMap<String,Integer> theseCosts = transitCosts.get(sequence[pos]);
            String nextBuilding = sequence[pos+1];
            int stepCost = theseCosts.get(nextBuilding);
            totalCost += stepCost;
        }
        return totalCost;
    }

    public static ArrayList<String> getMinCost(ArrayList<ArrayList> permutations, ArrayList<String> buildingNames, String startBuilding, HashMap<String,HashMap> transitCosts, HashMap<String,Integer> returnCosts){
        int[] permCost = new int[permutations.size()];
        HashMap<String, Integer> initialCosts = transitCosts.get(startBuilding);

        //Each thread only covers a single iteration, so we only need to calculate
        //a single starting cost.
        int firstCost = initialCosts.get(buildingNames.get(0));

        //Loop across all permutations to calculate the cost of each
        for(int permCount=0;permCount<permutations.size();permCount++){
            permCost[permCount] = firstCost;
            ArrayList<String> thisPerm = permutations.get(permCount);

            //Loop through building to calculate the step-wise costs.
            for(int bldNum=0;bldNum < thisPerm.size()-1;bldNum++){
                HashMap<String,Integer> travelCosts = transitCosts.get(thisPerm.get(bldNum));
                int stepCost = travelCosts.get(thisPerm.get(bldNum+1));
                permCost[permCount] += stepCost;
            }

            //Calculate cost to return to initial building.
            String lastBld = thisPerm.get(thisPerm.size()-1);
            int lastCost = returnCosts.get(lastBld);
            permCost[permCount] += lastCost;
        }
        //Determine lowest cost permutation
        int minCost = Integer.MAX_VALUE;
        int permPos = 0;
        ArrayList<Integer> allMins = new ArrayList<Integer>();
        ArrayList<String> minPerms = new ArrayList<String>();

        for(int i = 0;i<permCost.length;i++){
            if (permCost[i] < minCost){
                minCost = permCost[i];
                permPos = i;
            }
        }

        //Find all permutations with this minimum.
        for(int i=0;i<permCost.length;i++){
            if(permCost[i] == permCost[permPos]){
                allMins.add(i);
            }
        }

        //Build String with cheapest permutation.  Append cost.
        for(int minPos : allMins){
            String answer = startBuilding;
            ArrayList<String> minPerm = permutations.get(minPos);
            for(String bld:minPerm){
                answer += " " + bld;
            }
            answer += " " + String.valueOf(permCost[minPos]);
            minPerms.add(answer);
        }
        return minPerms;
    }

    public static ArrayList<ArrayList> getperms(String[] thisSeq, String startBuilding, HashMap<String,HashMap> transitCosts){
        int minCost = Integer.MAX_VALUE;
        ArrayList<String[]> allPerms = new ArrayList<String[]>();
        int seqSize = thisSeq.length;
        String[] testSeq = new String[seqSize+2];
        testSeq[0] = startBuilding;
        testSeq[seqSize] = startBuilding;

        int[] status = new int[seqSize];
        int count = 1;

        //initialize array with 0
        for (int items=0;items < seqSize;items++){
            status[items] = 0;
        }

        int item = 0;
        while (item < seqSize){
            if(status[item] < item){
                String temp = thisSeq[item];
                if (item % 2 == 0){
                    thisSeq[item] = thisSeq[0];
                    thisSeq[0] = temp;
                } else {
                    thisSeq[item] = thisSeq[status[item]];
                    thisSeq[status[item]] = temp;
                } //if item is even

                for(int dc = 1;dc<=seqSize;dc++){
                    testSeq[dc] = thisSeq[dc-1];
                }

                //thisSeq is now a new sequenc.
                int seqCost = getSingleCost(thisSeq,transitCosts);
                if(seqCost < minCost){
                    minCost = seqCost;
                    allPerms.add(thisSeq);
                }
                count += 1;
                status[item] += 1;
                item = 0;
            } else {
                status[item] = 0;
                item += 1;
            } //if status < item
        } //while

        return allPerms;
    }

    public static ArrayList<ArrayList> getPermutations(ArrayList<String> thisSeq, boolean isTop){
        ArrayList<ArrayList> allSeq = new ArrayList<ArrayList>();
        ArrayList<ArrayList> multiSeq = new ArrayList<ArrayList>();

        //Loop through current sequence
        for(int index=0;index < thisSeq.size();index++){

            //If we have a sequence larger than 2, we use recursion to reduce
            if(thisSeq.size() > 2){
                ArrayList<String> newSeq = new ArrayList<String>();
                for(int i=1;i<thisSeq.size();i++){
                    newSeq.add(thisSeq.get(i));
                }
                multiSeq = getPermutations(newSeq,false);

            } else {
                //Build base sequences of two items.
                ArrayList<String> seq0 = new ArrayList<String>();
                ArrayList<String> seq1 = new ArrayList<String>();
                seq0.add(thisSeq.get(0));
                seq0.add(thisSeq.get(1));
                seq1.add(thisSeq.get(1));
                seq1.add(thisSeq.get(0));
                ArrayList<ArrayList> bothSeq = new ArrayList<ArrayList>();
                bothSeq.add(seq0);
                bothSeq.add(seq1);

                return bothSeq;
            } //if-else

            //Append the first item from the parent sequenct to the child sequences
            //Add the cost of first item for each sub sequence
            for(int seqCount=0;seqCount < multiSeq.size();seqCount++) {
                ArrayList<String> newSeq = new ArrayList<String>();
                ArrayList<String> thisMultiSeq = new ArrayList<String>();
                newSeq.add(thisSeq.get(0));
                thisMultiSeq = multiSeq.get(seqCount);
                for (String thisBuilding : thisMultiSeq) {
                    newSeq.add(thisBuilding);
                }
                allSeq.add(newSeq);
            }  //for multiseq

            //Each thread only works on 1 set of sequences
            if(isTop){
                break;
            }

            thisSeq.add(thisSeq.remove(0));
        } //for seq loop

        return allSeq;
    }

    public void run() {
        //Generate permutations
        ArrayList<String[]> allResults = getperms(newSequence, startBuilding, allCosts);

        //Calculate cost of permutations

        //Return answer to runner.  Answer may include more than one perm.
        String onePerm = minTravel.get(0);
        String[] answerParts = onePerm.split(" ");
        int minCost = Integer.parseInt(answerParts[answerParts.length-1]);
        FinalRunner.minPath(minCost,minTravel);
    }

}



