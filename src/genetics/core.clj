(ns genetics.core
  (:import (java.lang Math)))

;; #Genetic Mapping

(def human-recombination-freq
  "The number of base pairs represented by one centimorgan" 1.0e6)

(defn compute-distance
  "Computes the distance between loci l1 and l2 given the observed
  genotype frequencies for f2, assuming a cross between a
  heterozygote parent and a homozygote parent."
  [l1 l2 population]
  ; this is not filled in yet
    1.0)

;; #Population Genetics

(defn doubling-time
  "Computes the time required for a population to double, given the
  growth rate r."
  [r]
  (/ log 2) r)


(defn population-growth
  "Computes the size of a population after time t given the growth
  rate r and the initial population size n."
  [n r t]
  (* n (exp (* r t))))


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
  "Computes the relative fitness of each genotype."
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



