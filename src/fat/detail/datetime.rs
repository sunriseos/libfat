#[derive(Debug)]
pub struct FatDateTime {
    year: u16,
    month: u8,
    day: u8,

    hour: u8,
    minutes: u8,
    seconds: u8,

    tenths: u8,
}

impl FatDateTime {
    const DAYS: [[u16; 12]; 4] = [
        [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335],
        [366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670, 700],
        [
            731, 762, 790, 821, 851, 882, 912, 943, 974, 1004, 1035, 1065,
        ],
        [
            1096, 1127, 1155, 1186, 1216, 1247, 1277, 1308, 1339, 1369, 1400, 1430,
        ],
    ];
    pub fn new(
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minutes: u8,
        seconds: u8,
        tenths: u8,
    ) -> Self {
        FatDateTime {
            year,
            month,
            day,
            hour,
            minutes,
            seconds,
            tenths,
        }
    }

    pub fn to_unix_time(&self) -> u64 {
        // TODO: support other ranges than 2000-2099
        if self.year > 2099 || self.year < 2000 {
            return 0;
        }

        let year = u64::from(self.year) % 100;
        let month = u64::from(self.month) - 1;
        let day = u64::from(self.day) - 1;

        let hour = u64::from(self.hour);
        let minutes = u64::from(self.minutes);
        let seconds = u64::from(self.seconds);

        946_684_800
            + (((year / 4 * (365 * 4 + 1)
                + u64::from(Self::DAYS[year as usize % 4][month as usize])
                + day)
                * 24
                + hour)
                * 60
                + minutes)
                * 60
            + seconds
    }
}