//modified from
//https://raw.githubusercontent.com/mathias-brandewinder/Machine-Learning-In-Action/master/MachineLearningInAction/MachineLearningInAction/KMeansClustering.fs
namespace MachineLearning

module KMeansClustering =
    let MAX_ITER = 20
    type Centroid<'a> = 'a * 'a list
    // the Distance between 2 observations 'a is a float
    // It also better be positive - left to the implementer
    type Distance<'a> = Centroid<'a> -> 'a -> float
    // CentroidsFactory, given a dataset, 
    // should generate n Centroids
    type CentroidsFactory<'a> = 'a seq -> int -> Centroid<'a> list
    // Given a Centroid and 
    type ToCentroid<'a> = Centroid<'a> -> 'a list -> Centroid<'a>

    // Returns the index of and distance to the 
    // Centroid closest to observation
    let closest (dist: Distance<'a>) centroids (obs: 'a) =
        centroids
        |> Seq.mapi (fun i c -> (i, dist c obs)) 
        |> Seq.minBy (fun (i, d) -> d)

    // Euclidean distance between 2 points, represented as float []
    let euclidean x y = 
        Array.fold2 (fun d e1 e2 -> d + pown (e1 - e2) 2) 0. x y 
        |> sqrt

    // Picks k random observations as initial centroids
    // (this is very lazy, even tolerates duplicates)
    let randomCentroids<'a> (rng: Probability.XorshiftPRNG) 
                            (sample: 'a seq) 
                            k =
        let size = Seq.length sample
        seq { for i in 1 .. k do 
              let pick = Seq.item (rng.Next(size)) sample
              yield pick } |> Seq.toList

    // Recompute Centroid as average of given sample
    let avgCentroid (current: float []) (sample: float [] seq) =
        let size = Seq.length sample
        match size with
        | 0 -> current
        | _ ->
            sample
            |> Seq.reduce (fun v1 v2 -> 
                   Array.map2 (fun v1x v2x -> v1x + v2x) v1 v2)
            |> Array.map (fun e -> e / (float)size)

    // Given a distance, centroid factory and
    // centroid aggregation function, identify
    // the k centroids of a dataset
    let kmeans (dist: Distance<'a>) 
               (factory: CentroidsFactory<'a>) 
               (aggregator: ToCentroid<'a>)
               (dataset: 'a seq) 
               k =
        // Recursively update Centroids and
        // the assignment of observations to Centroids
        let rec update count (centroids, assignment) =
            // Assign each point to the closest centroid
            let next = 
                dataset 
                |> Seq.map (fun obs -> closest dist centroids obs)
                |> Seq.toList
            // Check if any assignment changed
            let change =
                match assignment with
                | Some(previous) -> 
                    Seq.zip previous next    
                    |> Seq.exists (fun ((i, _), (j, _)) -> not (i = j))
                | None -> true // initially we have no assignment
            if change && (count < MAX_ITER)
            then 
                // Update each Centroid position:
                // extract cluster of points assigned to each Centroid
                // and compute the new Centroid by aggregating cluster
                let updatedCentroids =
                    let assignedDataset = Seq.zip dataset next
                    centroids 
                    |> Seq.mapi (fun i centroid -> 
                        assignedDataset 
                        |> Seq.filter (fun (_, (ci, _)) -> ci = i)
                        |> Seq.map (fun (obs, _) -> obs)
                        |> Seq.toList
                        |> aggregator centroid)
                // Perform another round of updates
                update (count+1) (updatedCentroids, Some(next))
            // No assignment changed, we are done
            else (centroids, next)

        let initialCentroids = factory dataset k
        let centroids = update 0 (initialCentroids, None) |> fst |> Seq.toList        
        let classifier = fun datapoint -> 
            centroids 
            |> List.minBy (fun centroid -> dist centroid datapoint)
        centroids, classifier