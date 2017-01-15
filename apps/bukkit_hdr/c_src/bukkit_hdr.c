#include <errno.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "bukkit_hdr.h"

static int64_t power(int64_t base, int64_t exp)
{
  int64_t result = 1;
  while (exp) {
    result *= base;
    exp--;
  }

  return result;
}

static int32_t get_bucket_index(Hdr *hdr, int64_t value)
{
  // Smallest power of 2 containing the value
  int32_t pow2ceiling = 64 - __builtin_clzll(value | hdr->sub_bucket_mask);
  int32_t magnitude = hdr->unit_magnitude;
  int32_t sbhcm = hdr->sub_bucket_half_count_magnitude + 1;
  return pow2ceiling - magnitude - sbhcm;
}

static int32_t get_sub_bucket_index(int64_t v, int32_t index, int32_t magnitude)
{
  return (int32_t) (v >> (index + magnitude));
}

static int32_t counts_index(Hdr *hdr, int32_t bucket_index, int32_t sub_index)
{
  int32_t sbhcm = hdr->sub_bucket_half_count_magnitude;
  int32_t bucket_base_index = (bucket_index + 1) << sbhcm;
  int32_t offset_in_bucket = sub_index - hdr->sub_bucket_half_count;
  return bucket_base_index + offset_in_bucket;
}

static int32_t normalize_index(Hdr *hdr, int32_t index)
{
  if (hdr->normalizing_index_offset == 0) {
    return index;
  }

  int32_t normalized_index = index - hdr->normalizing_index_offset;
  int32_t adjustment = 0;

  if (normalized_index < 0) {
    adjustment = hdr->counts_len;
  } else if (normalized_index >= hdr->counts_len) {
    adjustment = -hdr->counts_len;
  }

  return normalized_index + adjustment;
}

static int64_t size_of_equivalent_value_range(Hdr *hdr, int64_t value)
{
  int32_t bucket_index = get_bucket_index(hdr, value);
  int32_t sub_bucket_index = get_sub_bucket_index(
    value,
    bucket_index,
    hdr->unit_magnitude
  );

  if (sub_bucket_index >= hdr->sub_bucket_count) {
    bucket_index++;
  }

  return INT64_C(1) << (hdr->unit_magnitude + bucket_index);
}

static int64_t value_from_index(int32_t index, int32_t sub_index, int32_t mag)
{
  return ((int64_t) sub_index) << (index + mag);
}

static int64_t value_at_index(Hdr *hdr, int32_t index)
{
  int32_t bucket_index = (index >> hdr->sub_bucket_half_count_magnitude) - 1;
  int32_t sub_bucket_index = (index & (hdr->sub_bucket_half_count - 1));

  if (bucket_index < 0) {
    bucket_index = 0;
  } else {
    sub_bucket_index += hdr->sub_bucket_half_count;
  }

  return value_from_index(bucket_index, sub_bucket_index, hdr->unit_magnitude);
}

static int64_t lowest_equivalent_value(Hdr *hdr, int64_t value)
{
  int32_t bucket_index = get_bucket_index(hdr, value);
  int32_t sub_bucket_index = get_sub_bucket_index(
    value,
    bucket_index,
    hdr->unit_magnitude
  );
  return value_from_index(bucket_index, sub_bucket_index, hdr->unit_magnitude);
}

static int64_t highest_eqivalent_value(Hdr *hdr, int64_t value)
{
  int64_t ret = lowest_equivalent_value(hdr, value);
  ret += size_of_equivalent_value_range(hdr, value);
  return ret - 1;
}

static int64_t median_equivalent_value(Hdr *hdr, int64_t value)
{
  int64_t ret = lowest_equivalent_value(hdr, value);
  ret += (size_of_equivalent_value_range(hdr, value) >> 1);
  return ret;
}

int bukkit_hdr_new(int64_t lowest, int64_t highest, int sigfig, Hdr **result)
{

  Hdr *hdr = NULL;
  hdr = calloc(1, sizeof(Hdr));
  if (hdr == NULL) {
    return ENOMEM;
  }

  hdr->lowest_trackable_value = lowest;
  hdr->highest_trackable_value = highest;
  hdr->significant_figures = sigfig;
  int32_t mag = (int32_t) floor(log((double) lowest) / log(2));
  hdr->unit_magnitude = mag;

  // Largest single resolution value
  int64_t lsrv =  2 * power(10, sigfig);
  int32_t sbcm = ceil(log((double) lsrv) / log(2));
  int32_t sbhcm = ((sbcm > 1) ? sbcm : 1) - 1;
  hdr->sub_bucket_half_count_magnitude = sbhcm;
  hdr->sub_bucket_count = (int32_t) power(2, sbhcm + 1);
  hdr->sub_bucket_half_count = hdr->sub_bucket_count / 2;
  hdr->sub_bucket_mask = ((int64_t) hdr->sub_bucket_count - 1) << mag;

  hdr->bucket_count = 1;
  int64_t smallest_untrackable_value = ((int64_t) hdr->sub_bucket_count) << mag;
  while (smallest_untrackable_value <= highest) {
    hdr->bucket_count++;
    if (smallest_untrackable_value > INT64_MAX / 2) {
      break;
    }
    smallest_untrackable_value <<= 1;
  }
  hdr->counts_len = (hdr->bucket_count + 1) * (hdr->sub_bucket_count / 2);

  hdr->min = INT64_MAX;
  hdr->max = 0;
  hdr->normalizing_index_offset = 0;
  hdr->conversion_ratio = 1.0;
  hdr->total_count = 0;

  hdr->counts = calloc(hdr->counts_len, sizeof(int64_t));
  if (hdr->counts == NULL) {
    free(hdr);
    return ENOMEM;
  }

  *result = hdr;
  return 0;
}

void bukkit_hdr_free(Hdr *hdr)
{
  free(hdr->counts);
  free(hdr);
  return;
}

int bukkit_hdr_add(Hdr *from, Hdr *to)
{
  for (int32_t index = 0; index < from->counts_len; index++) {
    int64_t value = value_at_index(from, index);
    int64_t count = from->counts[normalize_index(from, index)];
    for (int i = 0; i < count; i++) {
      if (bukkit_hdr_update(to, value) != 0) {
        return 1;
      }
    }
  }

  return 0;
}

int bukkit_hdr_update(Hdr *hdr, int64_t value)
{
  if (value < hdr->lowest_trackable_value) {
    return EINVAL;
  }

  int32_t mag = hdr->unit_magnitude;
  int32_t bucket_index = get_bucket_index(hdr, value);
  int32_t sub_bucket_index = get_sub_bucket_index(value, bucket_index, mag);
  int32_t index = counts_index(hdr, bucket_index, sub_bucket_index);
  if (index < 0 || hdr->counts_len <= index) {
    return EINVAL;
  }

  int32_t normalized_index = normalize_index(hdr, index);
  hdr->counts[normalized_index]++;
  hdr->total_count++;
  hdr->min = (value < hdr->min) ? value : hdr->min;
  hdr->max = (value > hdr->max) ? value : hdr->max;
  return 0;
}

int bukkit_hdr_read(Hdr *hdr, HdrRead **result)
{
  HdrRead *read = calloc(1, sizeof(HdrRead));
  if (read == NULL) {
    return ENOMEM;
  }

  if (hdr->counts[normalize_index(hdr, 0)] > 0) {
    read->min = 0;
  } else if (hdr->min == INT64_MAX) {
    read->min = INT64_MAX;
  } else {
    read->min = lowest_equivalent_value(hdr, hdr->min);
  }

  if (hdr->max == 0) {
    read->max = 0;
  } else {
    read->max = highest_eqivalent_value(hdr, hdr->max);
  }

  read->p50 = bukkit_hdr_percentile(hdr, 50.0);
  read->p75 = bukkit_hdr_percentile(hdr, 75.0);
  read->p90 = bukkit_hdr_percentile(hdr, 90.0);
  read->p99 = bukkit_hdr_percentile(hdr, 99.0);
  read->p999 = bukkit_hdr_percentile(hdr, 99.9);

  read->mean = bukkit_hdr_mean(hdr);
  read->stddev = bukkit_hdr_stddev(hdr);

  *result = read;
  return 0;
}

void bukkit_hdr_read_free(HdrRead *read)
{
  free(read);
  return;
}

int64_t bukkit_hdr_percentile(Hdr *hdr, double percentile)
{
  percentile = percentile < 100.0 ? percentile : 100.0;
  int64_t count = (int64_t) (percentile / 100 * hdr->total_count + 0.5);
  int64_t total = 0;

  for (int32_t index = 0; index < hdr->counts_len; index++) {
    total += hdr->counts[normalize_index(hdr, index)];
    if (total > count) {
      return highest_eqivalent_value(hdr, value_at_index(hdr, index));
    }
  }

  return 0;
}

double bukkit_hdr_mean(Hdr *hdr)
{
  if (hdr->total_count == 0) {
    return NAN;
  }

  int64_t total = 0;
  for (int32_t index = 0; index < hdr->counts_len; index++) {
    int64_t count = hdr->counts[normalize_index(hdr, index)];
    total += count * median_equivalent_value(hdr, value_at_index(hdr, index));
  }

  return (total * 1.0) / hdr->total_count;
}

double bukkit_hdr_stddev(Hdr *hdr)
{
  if (hdr->total_count == 0) {
    return NAN;
  }

  double mean = bukkit_hdr_mean(hdr);
  double variance = 0.0;

  for (int32_t index = 0; index < hdr->counts_len; index++) {
    int64_t value = median_equivalent_value(hdr, value_at_index(hdr, index));
    double dev = (value * 1.0) - mean;
    variance += (dev * dev) * hdr->counts[normalize_index(hdr, index)];
  }

  return sqrt(variance / hdr->total_count);
}
