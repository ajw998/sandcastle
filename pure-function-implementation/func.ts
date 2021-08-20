export function reduce<T, K>(arr: T[], fn: (acc: K, value: T ) => K, initialValue: K): K {
  if (arr.length === 0) return initialValue;
  
  const [first, ...rest] = arr;
  
  return reduce(rest, (initialValue, first) => fn(initialValue, first), fn(initialValue, first))
}

export function map<T>(arr: T[], fn: (value: T) => T): T[] {
  return reduce(arr, (acc: T[], cur: T) => [...acc, fn(cur)], [])
}

export function filter<T>(arr: T[], fn: (value: T) => Boolean): T[] {
  return reduce(arr, (acc: T[], cur: T) => [...acc, ...(fn(cur) ? [cur] : [])], [])
  
}

export function every<T>(arr: T[], fn: (value: T) => Boolean): Boolean {
  return reduce(arr, (acc: Boolean, cur: T) => fn(cur) && acc, true)
}


export function find<T>(arr: T[], fn: (value: T) => Boolean): T | null {
    return reduce(arr, (acc: T | null, cur: T) => fn(cur) ? cur : acc, null)
}
