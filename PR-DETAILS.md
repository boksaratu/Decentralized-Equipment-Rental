# Equipment Availability Calendar

## Overview
Enhanced the Decentralized Equipment Rental platform with an independent Equipment Availability Calendar feature that allows equipment owners to proactively manage availability windows and set blackout periods for their equipment. This feature provides better planning capabilities and prevents rental conflicts.

## Technical Implementation
**New Contract**: `Equipment-Availability-Calendar.clar`
- **Ownership Management**: Equipment registration system for independent operation
- **Availability Windows**: Set specific periods when equipment is available/unavailable
- **Blackout Periods**: Define maintenance periods or temporary unavailability  
- **Smart Validation**: Block-height based scheduling with conflict detection
- **Query Functions**: Check availability status and calculate maximum rental periods

**Key Functions Added**:
- `register-equipment()` - Register equipment for availability management
- `set-availability-window()` - Define availability periods with notes
- `set-blackout-period()` - Block specific time periods with reasons
- `is-equipment-available-for-period()` - Check availability for rental periods
- `calculate-available-rental-days()` - Find maximum available rental duration

**Data Structures**:
- Equipment ownership registry with principal mapping
- Availability windows with start/end blocks and availability flags
- Blackout periods with maintenance reasons and scheduling
- Atomic counters for window and period ID management

## Testing & Validation
- ? Contract passes `clarinet check` with only minor unchecked data warnings
- ? All npm tests successful (1/1 passed)
- ? CI/CD pipeline configured with GitHub Actions
- ? Clarity v3 compliant with proper error constants and data types