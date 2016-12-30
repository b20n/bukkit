#ifndef BUKKIT_HDR_H
#define BUKKIT_HDR_H

#include <stdint.h>

typedef struct {
  int64_t lowest_trackable_value;
  int64_t highest_trackable_value;
  int32_t significant_figures;
  int32_t sub_bucket_half_count_magnitude;
  int32_t unit_magnitude;
  int32_t sub_bucket_count;
  int32_t sub_bucket_half_count;
  int64_t sub_bucket_mask;
  int32_t counts_len;
  int32_t bucket_count;
  int64_t min;
  int64_t max;
  int32_t normalizing_index_offset;
  double conversion_ratio;
  int64_t total_count;
  int64_t *counts;
} Hdr;

typedef struct {
  int64_t min;
  int64_t max;
  int64_t p50;
  int64_t p75;
  int64_t p90;
  int64_t p99;
  int64_t p999;
  double mean;
  double stddev;
} HdrRead;

int bukkit_hdr_new(int64_t, int64_t, int, Hdr**);
void bukkit_hdr_free(Hdr*);
int bukkit_hdr_add(Hdr*, Hdr*);
int bukkit_hdr_update(Hdr*, int64_t);
int bukkit_hdr_read(Hdr*, HdrRead**);
int64_t bukkit_hdr_percentile(Hdr*, double);
double bukkit_hdr_mean(Hdr*);
double bukkit_hdr_stddev(Hdr*);

#endif
