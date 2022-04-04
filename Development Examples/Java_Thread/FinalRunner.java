//This code only works with up to 12 buildings.
//13 buildings causes this to fail.

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class FinalRunner {

    private static int minCost = Integer.MAX_VALUE;
    private static ArrayList<String> minRoutes = new ArrayList<String>();

    public synchronized static void minPath(int threadCost,ArrayList<String> threadRoutes){
        //System.out.println(threadRoute);
        if(threadCost < minCost){
            minCost = threadCost;
            minRoutes = threadRoutes;
        }
    }

    public static ArrayList<ArrayList> shuffleCosts(ArrayList<ArrayList> currentCosts){
        //Reorder the costs based on the new building list order
        for(ArrayList<Integer> bldcosts:currentCosts){
            bldcosts.add(bldcosts.remove(0));
        }
        currentCosts.add(currentCosts.remove(0));
        return currentCosts;
    }

    public static int canUse(int remaining, int threads){
        //Determine how many threads I should create
        //Based on items remaining and threadcount.
        int usage = 0;
        if (remaining > threads){
            usage = threads;
        } else {
            usage = remaining;
        }
        return usage;
    } //canUse method

    public static void main(String[] args) throws InterruptedException {
        String strDir = System.getProperty("user.dir");
        System.out.println("Reading input2.txt from dir " + strDir);
        Path fileDir = Paths.get(System.getProperty("user.dir"));
        Path inputFile = Paths.get(fileDir.toString(), "input2.txt");
        Path outputFile = Paths.get(fileDir.toString(), "output2.txt");

        ArrayList<String> buildings = new ArrayList();
        ArrayList<Integer> allCosts = new ArrayList();

        long startTime = System.currentTimeMillis();

        //Read tripcost file and copy into arraylists.
        try {
            Scanner tripCosts = new Scanner(inputFile);
            while (tripCosts.hasNextLine()) {
                String thisLine = tripCosts.nextLine();
                String[] dataParts = thisLine.split(":");

                //Get building
                dataParts[0] = dataParts[0].trim();
                buildings.add(dataParts[0]);

                //Get costs
                dataParts[1] = dataParts[1].trim();
                String[] costString = dataParts[1].split(" ");
                for(int i=0;i<costString.length;i++){
                    int thisCost = Integer.parseInt(costString[i]);
                    allCosts.add(thisCost);
                }
            } //while nextline
        } catch (Exception e) {
            System.out.println(e);
        }

        //Put costs in HashMap + Build end values
        int costCount = 0;
        HashMap<String,HashMap> allCostsHM = new HashMap<String, HashMap>();
        HashMap<String, Integer> endCosts = new HashMap<String,Integer>();
        for(int bldNum = 0;bldNum < buildings.size();bldNum++){
            HashMap<String,Integer> eachCost = new HashMap<String,Integer>();
            for(int bldNum2 = 0;bldNum2 < buildings.size();bldNum2++){
                eachCost.put(buildings.get(bldNum2),allCosts.get(costCount));
                if(bldNum >0 & bldNum2 == 0){
                    endCosts.put(buildings.get(bldNum),allCosts.get(costCount));
                }
                costCount++;
            }
            allCostsHM.put(buildings.get(bldNum),eachCost);
        }

        //Break up building and cost lists for threads.
        String startBuilding = buildings.get(0);
        buildings.remove(0);

        //Build Array of buildings - needed for clean ref passed to threads.
        ArrayList<ArrayList> building_perms = new ArrayList<ArrayList>();
        for (int i=0;i<buildings.size();i++){
            ArrayList<String> newOrder = new ArrayList<String>();
            for(String bld:buildings){
                newOrder.add(bld);
            }
            building_perms.add(newOrder);
            buildings.add(buildings.remove(0));
        }

        //Loop through buildings - pass details to thread.
        int threadCount = 2;
        FinalThread3[] workers2 = new FinalThread3[threadCount];
        int remaining = buildings.size();
        int usePerms = canUse(remaining,threadCount);
        int buildingCount = 0;
        while(remaining > 0){
            int nothing = 0;
            for(int i = 0;i < usePerms;i++){
                String strbld = "";
                for(String bld:buildings){
                    strbld += bld + " ";
                }
                workers2[i] = new FinalThread3(building_perms.get(buildingCount), startBuilding, endCosts,allCostsHM);
                workers2[i].start();
                buildingCount ++;
                remaining--;
            }
            //join all threads
            for (int i=0;i<usePerms;i++) {
                if (workers2[i].isAlive()) {
                    workers2[i].join();
                }
            }
            usePerms = canUse(remaining,threadCount);
        } //while loop through thread

        //Output Results (in console + file)
        long endtime = System.currentTimeMillis();
        long delta = endtime-startTime;
        for(String oneRoute:minRoutes){
            System.out.println(oneRoute);
        }
        System.out.println(delta);

        try{
            FileWriter output = new FileWriter(String.valueOf(outputFile));
                for(String oneRoute:minRoutes) {
                    oneRoute += "\n";
                    output.write(oneRoute);
                }
            output.close();
        } catch (IOException e) {
            System.out.println(e);
        }

    } //main

}
