(ns genetics.core
  (:require clojure.set
            [clojure.string :only (lower-case upper-case)]))


;; #Genetic Mapping

(def human-recombination-freq
  "The number of base pairs represented by one centimorgan" 1.0e6)


(defn is-dominant?
  "Returns true iff the provided allele keyword is dominant (i.e., is
  represented by an upper-case keyword)."
  [allele]
  (= (name allele) (clojure.string/upper-case (name allele))))


(defn is-recessive?
  "Returns true iff the provided allele is recessive (i.e., is
  represented by a lower-case keyword)"
  [allele]
  (not (is-dominant? allele)))


(defn find-dominants
  "Returns the set of dominant alleles in the genotype"
  [genotype]
  (set (filter #(is-dominant? %) genotype)))


(defn find-recessives
  "Returns the set of recessives alleles in the genotype"
  [genotype]
  (set (filter #(is-recessive? %) genotype)))


(defn allele-type
  "Returns :dominant or :recessive depending on the type
  of the provided allele"
  [allele]
  (if (is-dominant? allele) :dominant :recessive))


(defmulti dominant-form
  "Returns the dominant form of the provided allele."
  class)

(defmethod dominant-form String [allele]
  (keyword (clojure.string/upper-case allele)))

(defmethod dominant-form clojure.lang.Keyword [allele]
  (dominant-form (name allele)))


(defmulti recessive-form
  "Returns the recessive form of the provided allele."
  class)

(defmethod recessive-form String [allele]
  (keyword (clojure.string/lower-case allele)))

(defmethod recessive-form clojure.lang.Keyword [allele]
  (recessive-form (name allele)))


(def progeny
  { #{:A :B :C} 479
    #{:a :b :c} 473
    #{:A :b :c} 15
    #{:a :B :C} 13
    #{:A :B :c} 9
    #{:a :b :C} 9
    #{:A :b :C} 1
    #{:a :B :c} 1})


(defn genotype
  "Converts a collection of alleles into a genotype map whose keys are
  the dominant form of the allele and whose values are the allele form
  present in the provided genotype"
  [alleles]
  (into {}
    (map #(vector (dominant-form %) (allele-type %)) alleles)))


(defn find-minority-allele-type
  "Returns the minority allele type and the alleles for the provided
  genotype"
  [genotype]
  (let [grouped-genotype (group-by allele-type genotype)]
    (apply min-key #(count (second %)) grouped-genotype)))


(defn minority-allele-type
  "Returns the minority allele type for the provided genotype"
  [genotype]
  (first (find-minority-allele-type [genotype])))


(defn minority-type-alleles
  "Returns the alleles whose type is in the minority for the provided
   genotype"
  [genotype]
  (second (find-minority-allele-type [genotype])))


(defn compute-distance
  "Computes the distance between loci l1 and l2 given the observed
  genotype frequencies for f2, assuming a test cross between a
  heterozygote parent and a homozygote parent."
  [l1 l2 population]
    (let [loci (vector (dominant-form l1) (dominant-form l2))
          pop-size (reduce + (vals population))]
    ; find the size of the populations where l1 and l2 are not both in
    ; the parental phase
      )
    1.0)

;; #Population Genetics

(defn doubling-time
  "Computes the time required for a population to double, given the
  growth rate r."
  [r]
  (/ (Math/log 2) r))


(defn population-growth
  "Computes the size of a population after time t given the growth
  rate r and the initial population size n."
  [n r t]
  (* n (Math/exp (* r t))))


(defn allele-frequency
  "Computes the allele frequency for a locus having
  2 distinct alleles."
  [n-aa n-ab n-bb]
  (let [total (float (+ n-aa n-ab n-bb))
        h-freq (/ n-ab 2)
        p (/ (+ n-aa h-freq) total)
        q (/ (+ n-bb h-freq) total)]
    [p q total]))


(defn find-genotype-frequency
  "Finds the genotype frequency for a single locus with 2
  distinct alleles a and b, given the number of homozygotes
  for a and b as well as the number of heterozygotes."
  [n-aa n-ab n-bb]
  (let [[p q total] (allele-frequency n-aa n-ab n-bb)]
    { :aa (/ n-aa total)
      :ab (/ n-ab total)
      :bb (/ n-bb total)}))


(defn find-hardy-weinberg-equilibrium
  "Computes the Hardy-Weinberg equilibrium for the provided allele
  frequency distribution; also returns the actual genotype frequency
  for the same population."
  [n-aa n-ab n-bb]
  (let [[p q total] (allele-frequency n-aa n-ab n-bb)]
    {:aa (* p p) :ab (* 2 p q) :bb (* q q)}))


(defn diff-genotype-frequency
  "Calculates the differences between the two genotype frequencies."
  [freq1 freq2]
  (letfn [(df [fg]
            (apply - (map fg (list freq1 freq2))))]
    {:aa (df :aa)
     :ab (df :ab)
     :bb (df :bb)}))


(defn relative-fitness
  "Computes the relative fitness of each genotype, given a map from
  genotypes to average number of surviving offspring."
  [genotype-fitness]
    (let [max-fitness (apply max (vals genotype-fitness))]
      (zipmap
        (keys genotype-fitness)
        (map #(/ % max-fitness) (vals genotype-fitness)))))


(defn one-gen-allele-freq-change
  "Calculates the magnitude of the average change in allele frequency
  after one generation of genetic drift in terms of allele frequencies
  p and q and the population size."
  [p q pop-size]
  (/ (* p q) (* 2 pop-size)))


(def standard-mu
  "The standard μ for per-base pair mutation rates." 1e-9)

(defn time-to-ancestor
  "Calculates the number of years separating 2 sequences given the
  mutation rate mu, the size of the sequences compared, and the number
  of SNPs found."
  [mu sample-size snps]
  (let [sample-mu (* mu sample-size)
        years-per-snp (/ 1 sample-mu)
        divergence (* snps years-per-snp)]
        (/ divergence 2)))



