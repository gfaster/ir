use std::{borrow::Borrow, ops::{IndexMut, Index}};

#[derive(Default)]
pub struct VecMap<K, V> {
    keys: Vec<K>,
    vals: Vec<V>,
}

impl<K, V> VecMap<K, V> {
    pub const fn new() -> Self {
        VecMap { keys: Vec::new(), vals: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.keys.len()
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&V> 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        let idx = self.keys.iter().position(|k| k.borrow() == key)?;
        Some(&self.vals[idx])
    }


    /// gets the index of key. Be aware that if an item is removed it the index may be invalidated.
    pub fn get_index<Q>(&self, key: &Q) -> Option<usize> 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        self.keys.iter().position(|k| k.borrow() == key)
    }

    pub fn contains_key<Q>(&self, key: &Q) -> bool 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        self.get(key).is_some()
    }

    pub fn get_key_val<Q>(&self, key: &Q) -> Option<(&K, &V)> 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        let idx = self.keys.iter().position(|k| k.borrow() == key)?;
        Some((&self.keys[idx], &self.vals[idx]))
    }

    pub fn get_mut<'a, Q>(&'a mut self, key: &'_ Q) -> Option<&'a mut V> 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        let idx = self.keys.iter().position(|k| k.borrow() == key)?;
        Some(&mut self.vals[idx])
    }

    pub fn insert(&mut self, key: K, val: V) -> Option<V> 
    where K: Eq
    {
        if let Some(idx) = self.keys.iter().position(|k| k == &key) {
            let ret = std::mem::replace(&mut self.vals[idx], val);
            Some(ret)
        } else {
            self.keys.push(key);
            self.vals.push(val);
            None
        }
    }

    pub fn remove<Q>(&mut self, key: &Q) -> Option<V> 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        let idx = self.keys.iter().position(|k| k.borrow() == key)?;
        self.keys.swap_remove(idx);
        Some(self.vals.swap_remove(idx))
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.keys.iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.vals.iter()
    }

    pub fn into_value_vec(self) -> Vec<V> {
        self.vals
    }
}

impl<K: Eq, V> Extend<(K, V)> for VecMap<K, V> {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<K, V, Q> std::ops::Index<&Q> for VecMap<K, V>
    where Q: Eq + ?Sized,
K: Eq + Borrow<Q>
{
    type Output = V;

    fn index(&self, index: &Q) -> &Self::Output {
        self.get(index).expect("key does not exist")
    }
}

impl<K, V, Q> std::ops::IndexMut<&Q> for VecMap<K, V>
    where Q: Eq + ?Sized,
K: Eq + Borrow<Q>
{
    fn index_mut(&mut self, index: &Q) -> &mut Self::Output {
        self.get_mut(index).expect("key does not exist")
    }
}

impl<K, V> IntoIterator for VecMap<K, V> {
    type Item = (K, V);

    type IntoIter = std::iter::Zip<std::vec::IntoIter<K>, std::vec::IntoIter<V>>;

    fn into_iter(self) -> Self::IntoIter {
        self.keys.into_iter().zip(self.vals)
    }
}

impl<'a, K, V> IntoIterator for &'a VecMap<K, V> {
    type Item = (&'a K, &'a V);

    type IntoIter = std::iter::Zip<std::slice::Iter<'a, K>, std::slice::Iter<'a, V>>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.keys).into_iter().zip(&self.vals)
    }
}

impl<'a, K, V> IntoIterator for &'a mut VecMap<K, V> {
    type Item = (&'a K, &'a mut V);

    type IntoIter = std::iter::Zip<std::slice::Iter<'a, K>, std::slice::IterMut<'a, V>>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.keys).into_iter().zip(&mut self.vals)
    }
}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for VecMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self).finish()
    }
}

#[derive(Default)]
pub struct VecSet<K>(Vec<K>);

impl<K: Eq> Extend<K> for VecSet<K> {
    fn extend<T: IntoIterator<Item = K>>(&mut self, iter: T) {
        for item in iter {
            self.insert(item);
        }
    }
}

impl<K> VecSet<K> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, key: K) -> bool
    where K: Eq
    {
        if self.0.contains(&key) {
            false
        } else {
            self.0.push(key);
            true
        }
    }

    pub fn contains<Q>(& self, key: &Q) -> bool 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        self.0.iter().find(|&x| x.borrow() == key).is_some()
    }

    pub fn remove<Q>(&mut self, key: &Q) -> bool 
    where K: Eq + Borrow<Q>,
    Q: Eq + ?Sized
    {
        if let Some(idx) = self.0.iter().position(|x| x.borrow() == key) {
            self.0.swap_remove(idx);
            true
        } else {
            false
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &K> {
        self.0.iter()
    }
}

impl<K> IntoIterator for VecSet<K> {
    type Item = K;

    type IntoIter = std::vec::IntoIter<K>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K> IntoIterator for &'a VecSet<K> {
    type Item = &'a K;

    type IntoIter = std::slice::Iter<'a, K>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for VecSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}
