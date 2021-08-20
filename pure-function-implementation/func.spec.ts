import { reduce, map, filter, every, find } from './func'

describe('reduce', () => {
  it('reduces the input', () => {
    expect(reduce([1, 2, 3], (acc: number, cur: number) => acc + cur, 0)).toBe(6)
  })
})

describe('map', () => {
  it('maps the input', () => {
    expect(map([1, 2, 3], (x: number) => x * 2)).toEqual([2, 4, 6])
  })
})

describe('filter', () => {
  it('filters the input', () => {
    expect(filter([1, 2, 3], x => x % 2 === 1)).toEqual([1, 3])
  })
})

describe('every', () => {
  it('returns true if all match', () => {
    expect(every([2, 4, 6], x => x % 2 === 0)).toBe(true)
  })
  
  it('returns false if one does not match', () => {
    expect(every([2, 4, 6, 7], x => x % 2 === 0)).toBe(false)
  })
})

describe('find', () => {
  it('returns the value if it is found', () => {
    expect(find([1, 2, 3], x => x === 2)).toBe(2)
  })
  
  it('returns null if the value is not found', () => {
    expect(find([1, 2, 3], x => x === 4)).toBe(null)
  })
})


