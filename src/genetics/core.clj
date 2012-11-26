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


(defn find-dominants
  "Returns the set of dominant alleles in the genotype"
  [genotype]
  (set (filter #(is-dominant? %) genotype)))


(defn find-recessives
  "Returns the set of recessives alleles in the genotype"
  [genotype]
  (set (filter #(is-recessive? %) genotype)))


(defn find-middle-gene
  [f2]
  (let [f2-v (sort-by #(nth % 1) (vec f2))
        [lf1 lf2 & rest] (vec f2-v)
        lf1-g (nth lf1 0)
        lf2-g (nth lf2 0)
       ;; this is still broken. you need to identify which are moving together
        odd-one-out (clojure.set/difference (find-dominants lf1-g) (find-recessives lf2-g))
        [middle] (vec odd-one-out)]
    (name middle)))


(defn compute-distance
  "Computes the distance between loci l1 and l2 given the observed
  genotype frequencies for f2, assuming a test cross between a
  heterozygote parent and a homozygote parent."
  [l1 l2 population]
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
  [μ sample-size snps]
  (let [sample-μ (* μ sample-size)
        years-per-snp (/ 1 sample-μ)
        divergence (* snps years-per-snp)]
        (/ divergence 2)))



